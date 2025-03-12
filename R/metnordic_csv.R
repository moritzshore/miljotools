#' Download MET Nordic data in CSV format
#'
#' note: this function only works for point geometry.
#'
#' This is a modified version of the normal `get_metno_reanalysis3` designed
#' specifically for harmonizing downloading in csv format and  netCDF. Since it
#' is quite hard to harmonize the netcdf stuff and the csv stuff with things
#' like point vs. polygon (and my bad coding practices), this function exists as
#' an intermediary where i try to step-wise harmonize the two methods as best as
#' i can. It hasnt fully worked out yet, but its like half way there.
#'
#' @param area (string) path to geo-referenced shapefile (point) of the desired area. (optionally, you can pass a `sf` object directly.)
#' @param path (string) path to download directory
#' @param fromdate (string) date and time for start of time series (ie. "2012-09-01 10:00:00")
#' @param todate (string) date and time for end of time series (ie. "2013-09-01 10:00:00")
#' @param mn_variables (vector) Leave blank for default variables
#' @param dataset (string) from which dataset to source the files from? ("reanalysis", "operational", or "continuous" for both).
#' @param verbose (boolean) generate graphs showing previews of data download?
#' @importFrom dplyr %>% as_tibble
#' @importFrom readr write_csv read_csv
#' @importFrom ncdf4 ncvar_get nc_close
#' @importFrom stringr str_split
#' @importFrom abind abind
#' @importFrom tibble is_tibble
#' @author Moritz Shore
#' @export
#' @return Function returns a path to where .csv files of the download were
#'   written. One .csv file for each grid point within the (buffered) shape
#'   file area. Additionally one metadata file (.csv) is written with the
#'
#' @examples
#'
#'  if(FALSE){
#'  metnordic_csv(
#'  area = example_file_path,
#'  fromdate = "2015-01-01",
#'  todate = "2015-01-02",
#'  verbose = TRUE
#'  )
#'  }
#'
#'

metnordic_csv <-
  function(area,
           path = NULL,
           fromdate =  "2012-09-01 10:00:00",
           todate = "2012-09-01 20:00:00",
           mn_variables = NULL,
           dataset = "continuous",
           verbose = TRUE
  ){

    ### Getting the bouding box
    mt_print(verbose, "metnordic_csv", "getting coordinates..")
    bounding_coords <- metnordic_coordwindow(area_path = area, verbose = verbose)

    ### Extracting the distance to point (if applicable), and saving for later
    # TODO: swich to return list instead of this method.
    metadist <- bounding_coords$metadist
    bounding_coords<- bounding_coords[-length(bounding_coords)]

    ### Building download queries
    mt_print(verbose, "metnordic_csv", "building query", paste0("from '", dataset, "' dataset"))
    queries <- metnordic_buildquery(
      bounding_coords = bounding_coords,
      mn_variables =  mn_variables,
      fromdate = fromdate,
      todate = todate,
      dataset = dataset,
      verbose = verbose)

    ### Creating download folder.
    if(path %>% is.null()){path = getwd()}
    if(dir.exists(path) == FALSE){dir.create(path)}

    foldername = basename(path)
    directory = dirname(path)
    mt_print(verbose, "metnordic_csv", "downloading here:\n", path)

    ### Downloading files:
    ## TODO: regular save intervals and restarting after failure.
    mt_print(verbose, "metnordic_csv", "starting download from dataset:", dataset)
    ncdownload <-
      csv_download_ncfiles(
        directory = directory,
        foldername = foldername,
        full_urls = queries$full_urls,
        filenames = queries$filenames,
        mn_variables = mn_variables,
        verbose = verbose
      )
    mt_print(verbose, "metnordic_csv", "download complete!")
    mt_print(verbose, "metnordic_csv", "writing station metadata to csv..")
    ### metdata file
    meta_df = tibble(
      lat = paste("lat = ", ncdownload$lat_crop),
      lon = paste("lon = ", ncdownload$lon_crop),
      x = paste("X =", ncdownload$x_crop),
      y = paste("Y =", ncdownload$y_crop),
      elevation = paste("ELEVATION = ", ncdownload$alt_crop),
      distance_to_gridcell = paste0("DISTANCE TO NEAREST GRIDCELL (in meters) = ", metadist),
      source = paste0(
        "Data sourced from MetNordic Reanalysis Dataset Meteorologisk institutt, downloaded by miljotools version ",
        utils::packageVersion("miljotools"), " on ", Sys.time()),
      link_metnordic = "https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset",
      link_miljotools = "https://moritzshore.github.io/miljotools"
    )

    meta2 <- meta_df %>% t() %>% as.data.frame()
    colnames(meta2) <- "METADATA"
    write_csv(x = meta2, file = paste0(directory, "/", foldername, "/METNORDIC_metadata.csv"), col_names = T)

    return(paste0(directory, "/",foldername))
  }
### Supporting Functions
csv_download_ncfiles <- function(directory,
                             foldername,
                             full_urls,
                             filenames,
                             mn_variables,
                             verbose = FALSE) {

  getncvar <- function(var, ncin_crop) {
    attempt = 0
    vals_crop = NA
    while ((attempt < 10) & (is.matrix(vals_crop)==FALSE)) {
      attempt = attempt + 1
      vals_crop <- tryCatch(
        expr = {
          ncvar_get(ncin_crop, var)
        },
        error = function(cond) {
          message(paste("error!", cond))
          Sys.sleep(10)
          return(NA)
        }
      )
    }
    return(vals_crop)
  }

  original_urls <- full_urls
  data_path <- paste0(directory, "/", foldername, "/METNORDIC_point.csv")
  log_path <- paste0(directory, "/", foldername, "/METNORDIC_log.csv")

  if(file.exists(log_path)){
    already_downloaded_urls <- read_csv(log_path, show_col_types = F)
    already_downloaded_urls <- already_downloaded_urls$written_urls
    already_downloaded_urls <- full_urls %in% already_downloaded_urls %>% which()
    full_urls <- full_urls[-already_downloaded_urls]
    warning("not downloading [", length(already_downloaded_urls), "] urls which have previously been written to disk.")
    append_flag = TRUE
  }else{
    append_flag = FALSE
  }

  if(full_urls %>% length()==0){
    stop("All files are already present in this directory! No need to download anything else!")
  }

  url <- full_urls
  ncin_crop <- nc_open_retry(url[1])

  # NOTE: Warning, this will break if the first file to donwload does not exist
  # this might be more common than one thinks, as a download-retry will first
  # attempt the failed files. Perhaps a better way to do this exists...
  # One idea would be to create a list of all missing files and then remove those
  # from the query.
  # download all the variables using custom function
  if(is.null(ncin_crop)){stop("first file the function tried to download failed, please remove this date from your request:\n", url[1])}


  # pre-download first frame to get dimensions set
  x_crop <- ncvar_get(ncin_crop,"x")
  y_crop <- ncvar_get(ncin_crop,"y")
  lon_crop <- ncvar_get(ncin_crop,"longitude")
  lat_crop <- ncvar_get(ncin_crop,"latitude")
  alt_crop <- ncvar_get(ncin_crop,"altitude")

  vardl <- lapply(mn_variables, getncvar, ncin_crop = ncin_crop)

  nc_close(ncin_crop)

  # set colnames
  names(vardl) <- mn_variables

  # predefile the master matrix
  mastermatrix <- vardl

  url_to_datetime <- function(url_1){
    dtcode = (url_1 %>% str_split("1_0km_nordic_"))[[1]][2] %>% substr(start = 0, stop = 11)
    year = substr(dtcode, 0,4)
    month = substr(dtcode, 5,6)
    day = substr(dtcode, 7,8)
    hour = substr(dtcode, 10,11)
    paste0(year, "-", month, "-", day, " ", hour, ":00:00") %>% return()
  }

  # write the inital file
  point_df <- lapply(mastermatrix, as.vector) %>% as.data.frame()
  date = url_to_datetime(url[1])
  final_df <- cbind(date = date, point_df) %>% dplyr::as_tibble()
  readr::write_csv(x = final_df, file = data_path,
                   append = append_flag, col_names = !append_flag)

  # write to log which files have already been downloaded.
  writen_urls <- data.frame(written_urls = url[1])
  readr::write_csv(x = writen_urls, file = log_path, append = append_flag, col_names = !append_flag)

  # repeat for all following files
  for (idate in c(2:length(url))) {
    # print status
    mt_print(verbose, "metnordic_csv", "downloading files:", paste0(url_to_datetime(url[idate])," (", idate, "/", length(url), ")"), rflag = TRUE)
    # open Netcdf file
    ncin_crop <- nc_open_retry(url[idate])

    if(is.null(ncin_crop) == FALSE){
      # download all vars
      vardl <- lapply(mn_variables, getncvar, ncin_crop = ncin_crop)
      ncdf4::nc_close(ncin_crop)
      names(vardl) <- mn_variables
      point_df <- lapply(vardl, as.vector) %>% as.data.frame()
      date = url_to_datetime(url[idate])
      final_df <- cbind(date = date, point_df) %>% dplyr::as_tibble()
      test = write_csv(x = final_df, file = data_path, append = T,col_names = F)

      if(tibble::is_tibble(test)){
        just_writen_urls <- data.frame(written_urls = url[idate])
        readr::write_csv(x = just_writen_urls, file = log_path, append = T)
      }
    }else{
      # procedure if the download failed: write an NA line
      nadf <- rep(NA, length(mn_variables))
      names(nadf) <- mn_variables
      point_df <- lapply(nadf, as.vector) %>% as.data.frame()
      date = url_to_datetime(url[idate])
      final_df <- cbind(date = date, point_df) %>% dplyr::as_tibble()
      write_csv(x = final_df, file = data_path, append = T,col_names = F)
      warning("failed to download url: \n", url[idate], "\n..skipping!")
    }
  } # for every day
  if (verbose) cat("\n")
  # so now we check if all files were downloaded
  dled <- read_csv(log_path, show_col_types = F)
  if(length(dled$written_urls) != length(original_urls)){
    notdled <- filenames[-(original_urls%in%dled$written_urls %>% which())]
    warning(
      "not all files were downloaded! The following have not been written to disk:\n",
      paste(notdled, collapse = "\n"))
  }else{
    mt_print(verbose, "metnordic_csv", "All files downloaded", paste0("[",length(original_urls), "]"))
  }
  return(list(mastermatrix = data_path,
              lat_crop = lat_crop,
              lon_crop = lon_crop,
              vardl = vardl,
              x_crop = x_crop,
              y_crop = y_crop,
              alt_crop = alt_crop))
}
