## TODO:
# Directly write data to disk, and auto-catch up when download restarts
# Add a custom foldername path
# Return geometry type in the list.
# Move crop/write station data to dedicated functions.

### Notes:
# EPGS code: 4368

# opendap protocol:
# https://thredds.met.no/thredds/dodsC/metpparchivev3/2023/01/31/met_analysis_1_0km_nordic_20230131T23Z.nc.html

# source data URL:
# https://thredds.met.no/thredds/catalog/metpparchivev3/catalog.html

# server status:
# https://status.met.no/


#' Download MET Nordic data in CSV format
#'
#' This is a modified version of the normal `get_metno_reanalysis3` designed
#' specifically for harmonizing downloading in csv format and  netCDF. Since it is quite
#' hard to harmonize the netcdf stuff and the csv stuff with things like point
#' vs. polygon (and my bad coding practices), this function exists as an
#' intermediary where i try to step-wise harmonize the two methods as best as i
#' can. It hasnt fully worked out yet, but its like half way there.
#'
#' @param area (string) path to geo-referenced shapefile (polygon or point) of the desired area. (optionally, you can pass a `sf` object directly.)
#' @param directory (string) path to desired working directory (default: working directory)
#' @param fromdate (string) date and time for start of time series (ie. "2012-09-01 10:00:00")
#' @param todate (string) date and time for end of time series (ie. "2013-09-01 10:00:00")
#' @param mn_variables (vector) Leave blank for default (tested) variables. See details for more
#' @param dataset (string) from which dataset to source the files from? ("reanalysis", "operational", or "continuous" for both).
#' @param area_buffer desired buffer around the provided shapefile (in meters)
#' @param grid_resolution (integer) desired resolution of downloaded grid in kilometers.
#' @param verbose generate graphs showing previews of data download? (Boolean)
#' @importFrom abind abind
#' @importFrom dplyr nth mutate %>% tibble
#' @importFrom lubridate year month day hour as_datetime
#' @importFrom mapview mapview
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom purrr map
#' @importFrom readr write_csv
#' @importFrom sf read_sf st_crs st_transform st_buffer st_bbox st_as_sf st_intersects st_coordinates st_zm
#' @importFrom stringr str_pad str_replace_all str_split
#' @importFrom mapview mapview
#' @importFrom crayon black bold green italic yellow blue
#'
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
#'  area_buffer = 100,
#'  verbose = TRUE
#'  )
#'  }
#'
#'

metnordic_csv <-
  function(area,
           directory = NULL,
           fromdate =  "2012-09-01 10:00:00",
           todate = "2012-09-01 20:00:00",
           mn_variables = NULL,
           dataset = "continuous",
           area_buffer = 0,
           grid_resolution = NULL,
           verbose = TRUE
  ){

    ### Getting the bouding box
    mt_print(verbose, "metnordic_csv", "getting coordinates..", paste0("with buffer of", area_buffer))
    bounding_coords <- metnordic_coordwindow(area_path = area, area_buffer =  area_buffer, verbose = verbose)

    ### Extracting the distance to point (if applicable), and saving for later
    # TODO: swich to return list instead of this method.
    metadist <- bounding_coords$metadist
    bounding_coords<- bounding_coords[-length(bounding_coords)]

    ### Determining Geometry type (BAD METHOD!, use return list of bounding_coords)
    if(bounding_coords %>% length() == 2){geometry_type = "point"}else{geometry_type = "polygon"}
    if (geometry_type == "polygon") stop("this is not supported yet in this function, please still use `get_metno_reanalysis3()`")

    ### Building download queries
    mt_print(verbose, "metnordic_csv", "building query", paste0("from ", dataset, "dataset and a resolution of ", grid_resolution, "km"))
    queries <- metnordic_buildquery(
      bounding_coords = bounding_coords,
      mn_variables =  mn_variables,
      fromdate = fromdate,
      todate = todate,
      grid_resolution = grid_resolution,
      dataset = dataset,
      verbose = verbose)

    ### Creating download folder.
    foldername <- csv_create_download_folder(directory)
    mt_print(verbose, "metnordic_csv", "creating download folder", foldername)


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
        geometry_type = geometry_type,
        verbose = verbose
      )
    mt_print(verbose, "metnordic_csv", "download complete!")

    # if the geometry type is polygon, we can delete all grid cells that are
    # not within the area.
    if (geometry_type == "polygon") {
      mt_print(verbose, "metnordic_csv", "cropping dataset to area coverage..")
      cover_stations <-
        csv_crop_dataset(
          lat_crop = ncdownload$lat_crop,
          lon_crop = ncdownload$lon_crop,
          area = bounding_coords$area_shp,
          area_buff = bounding_coords$area_buff,
          verbose = verbose
        )
      cat("\n") # for the warning..
    }

    mt_print(verbose, "metnordic_csv", "writing station data to csv..")

    # write procedure for polygon:
    if(geometry_type == "polygon"){
      csv_write_stations(
        vardl = ncdownload$vardl,
        cover_stations = cover_stations,
        mn_variables = mn_variables,
        x_crop = ncdownload$x_crop,
        y_crop = ncdownload$y_crop,
        lon_crop = ncdownload$lon_crop,
        lat_crop = ncdownload$lat_crop,
        alt_crop = ncdownload$alt_crop,
        mastermatrix = ncdownload$mastermatrix,
        daterange = queries$daterange,
        foldername = foldername,
        directory = directory,
        verbose = verbose,
        area = bounding_coords$area_shp
      )
    }else{ # write procedure for point.
      matrix <- ncdownload$mastermatrix
      point_df <- lapply(matrix, as.vector) %>% as.data.frame()
      date = seq(as.POSIXct(fromdate), as.POSIXct(todate), by = "hour") %>% strftime()
      final_df <- cbind(date, point_df) %>% dplyr::as_tibble()
      readr::write_csv(x = final_df, file = paste0(directory, "/",foldername, "/METNORDIC_point.csv"))

      ### metdata file
      meta_df = c(
        lat = paste("lat = ", ncdownload$lat_crop),
        lon = paste("lon = ", ncdownload$lon_crop),
        x = paste("X =", ncdownload$x_crop),
        y = paste("Y =", ncdownload$y_crop),
        elevation = paste("ELEVATION = ", ncdownload$alt_crop),
        distance_to_gridcell = paste0("DISTANCE TO NEAREST GRIDCELL (in meters) = ", metadist)
      )

      source = paste0(
        "Data sourced from MetNordic Reanalysis Dataset Meteorologisk institutt, downloaded by miljotools version ",
        utils::packageVersion("miljotools"),
        " on ",
        Sys.time()
      )

      fileConn <- file(paste0(directory, "/", foldername, "/metadata.txt"))
      writeLines(
        c(
          meta_df,
          source,
          "https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset",
          "https://moritzshore.github.io/miljotools"
        ),
        fileConn
      )
      close(fileConn)
    }
    return(paste0(directory, "/",foldername))
  }
### Supporting Functions
csv_create_download_folder <- function(directory){
  if(directory %>% is.null()){
    directory <- getwd()
  }

  # parsing date and time
  tad <-
    Sys.time() %>% stringr::str_replace_all("-", "") %>%
    stringr::str_replace_all(":", "") %>%
    stringr::str_replace_all(" ", "") %>%
    stringr::str_split("\\.", 2) %>% unlist() %>% dplyr::nth(1)
  # creating foldername
  foldername <- paste0("met_no_dl_",tad)
  # creating directory for download
  dir.create(paste0(directory, "/", foldername))

  return(foldername)
}
csv_download_ncfiles <- function(directory,
                             foldername,
                             full_urls,
                             filenames,
                             mn_variables,
                             geometry_type,
                             verbose = FALSE) {
  # helper function
  getncvar <- function(var, ncin_crop) {
    attempt = 0
    vals_crop = NA
    while ((attempt < 10) & (is.matrix(vals_crop)==FALSE)) {
      attempt = attempt + 1
      vals_crop <- tryCatch(
        expr = {
          ncdf4::ncvar_get(ncin_crop, var)
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

  url <- full_urls
  ncin_crop <- nc_open_retry(url[1])
  # pre-download first frame to get dimensions set
  x_crop <- ncdf4::ncvar_get(ncin_crop,"x")
  y_crop <- ncdf4::ncvar_get(ncin_crop,"y")
  lon_crop <- ncdf4::ncvar_get(ncin_crop,"longitude")
  lat_crop <- ncdf4::ncvar_get(ncin_crop,"latitude")
  alt_crop <- ncdf4::ncvar_get(ncin_crop,"altitude")

  # download all the variables using custom function
  vardl <- lapply(mn_variables, getncvar, ncin_crop = ncin_crop)

  ncdf4::nc_close(ncin_crop)

  # set colnames
  names(vardl) <- mn_variables

  # predefile the master matrix
  mastermatrix <- vardl

  # repeat for all following files
  for (idate in c(2:length(url))) {
    # print status
    mt_print(verbose, "metnordic_csv", "downloading files:", paste0("(", idate, "/", length(url), ")"), rflag = TRUE)
    # open Netcdf file
    ncin_crop <- nc_open_retry(url[idate])

    # download all vars
    vardl <- lapply(mn_variables, getncvar, ncin_crop = ncin_crop)

    ncdf4::nc_close(ncin_crop)

    # set column names
    names(vardl) <- mn_variables

    if(geometry_type == "point"){
      mat_dimension = 2

    }
    if(geometry_type == "polygon"){
      mat_dimension = 3
    }
    # for every variable, bind the matrix slice onto the full matrix (dimension 3 --> "along=3")
    for (variable in mn_variables) {
      # if the download failed, add a full frame of NAs to stack
      if (vardl[[variable]] %>% length() == 0) {
        dims <- mastermatrix[[variable]] %>% dim()
        na_frame <-
          matrix(data = NA,
                 nrow = dims[1],
                 ncol = dims[2])
        mastermatrix[[variable]] <-
          abind::abind(mastermatrix[[variable]], na_frame, along = mat_dimension)
      } else{
        mastermatrix[[variable]] <-
          abind::abind(mastermatrix[[variable]], vardl[[variable]], along = mat_dimension)
      } # end else
    } # for every var
  } # for every day

  if (verbose) cat("\n")
  return(list(mastermatrix = mastermatrix,
              lat_crop = lat_crop,
              lon_crop = lon_crop,
              vardl = vardl,
              x_crop = x_crop,
              y_crop = y_crop,
              alt_crop = alt_crop))
}

csv_crop_dataset <- function(lat_crop, lon_crop, area, area_buff, verbose){
  ### removing non touching points
  # create DF of points
  stations = data.frame(
    lat=lat_crop %>% as.vector(),
    lon=lon_crop %>% as.vector()
  )

  # create shape file from DF
  stations = sf::st_as_sf(stations, coords = c("lon","lat"), remove = FALSE)

  # set the geographic CRS
  sf::st_crs(stations) = sf::st_crs("+init=epsg:4326")

  # lambert conform conical (the projection used by met reanalysis)
  projection <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6371000"
  proj_crs <- sf::st_crs(projection) # replace with sf::crs()

  # transform to reanalysis projection
  stations <- sf::st_transform(stations, proj_crs)

  # figure out which ones are touching the area buffer
  pnts_trans <- stations %>% dplyr::mutate(
    intersection = as.integer(sf::st_intersects(stations, area_buff)))
  cover_stations <- stations[which(pnts_trans$intersection == 1),]

  # preview plot
  if(verbose){
    plot <- mapview::mapview(area)+mapview::mapview(stations)
    print(plot)
  }
  return(cover_stations)
}
csv_write_stations <- function(vardl, cover_stations, mn_variables, x_crop, y_crop,
                           lon_crop, lat_crop, alt_crop, mastermatrix,
                           daterange, foldername,directory,
                           verbose, area) {
  # getting dimensions of the x and y grid
  x_mat <- dim(vardl[[1]])[1]
  y_mat <- dim(vardl[[1]])[2]

  # total files to write
  i = 1 # iterator of file writing
  total_files <- length(cover_stations$lat)

  # predefine metadata DF
  metadata <-
    data.frame(
      ID = NA,
      Name = NA,
      Elevation = NA,
      Source = NA,
      Long = NA,
      Lat = NA
    )

  # this rounding thing is a not a great way of doing it, but also not a bad
  # way, as the results are near perfect. it might break though? consider redoing
  covercoords <- sf::st_coordinates(cover_stations) %>% as.data.frame()
  covercoordcart <- paste0("(", covercoords$X %>% round(0), ",",covercoords$Y %>% round(0), ")")

  for (xcell in c(1:x_mat)) {
    for (ycell in c(1:y_mat)) {

      # this rounding thing is a not a great way of doing it, but also not a bad
      # way, as the results are near perfect. it might break though? consider redoing
      statcoords <- paste0("(", x_crop[xcell] %>% round(0), ",", y_crop[ycell] %>% round(0), ")")

      # skip writing file if no coverage exists
      if(statcoords %in% covercoordcart){
        #print("yes")
      }else{
        #print("no, skipping")
        next()
      }

      # pre define master DF
      master_df <- data.frame(date = daterange)

      # building the time series data frame column for each variable
      for (variable in mn_variables) {
        # get the variable out of the list
        varslice <- mastermatrix[[variable]]
        # extract the timeseries for the given cell coordinates
        timeseries <- varslice[xcell,ycell,]
        ts_df <- data.frame(value = timeseries)
        # add the column to the master df
        master_df <- cbind(master_df, ts_df)
      }
      # get the lat-lon for the grid cell (for metadata.csv)
      # i need to double check that these are lat or lon
      clon <- lon_crop[xcell, ycell]
      clat <- lat_crop[xcell, ycell]

      # set the col names
      colnames(master_df) <- c("date", mn_variables)

      # get the altitude of the cell (for metadata.csv)
      altitude <- alt_crop[xcell, ycell]

      # generate a file name
      filename = paste0("sta_x",xcell,"_y", ycell,".csv")

      # add the metadata to the master file
      metadata_row <- data.frame(ID = paste0("ID",i),
                                 Name = paste0("sta_x",xcell,"_y", ycell),
                                 Elevation = altitude,
                                 Source = "",
                                 Long = clon, Lat = clat)
      metadata <- rbind(metadata, metadata_row)

      # iterate
      i = i + 1
      # status print
      mt_print(verbose, "metnordic_csv", text = "writing files...", paste0(i, "/" ,total_files+1), rflag = TRUE)
      # write the file
      readr::write_csv(master_df, file = paste0(directory, "/", foldername, "/", filename))
    }
  }

  # write the metadata
  cat("\n")
  mt_print(verbose, "metnordic_csv", "writing metadata..")
  readr::write_csv(metadata[-1,], file = paste0(directory, "/", foldername, "/metadata.csv"))

  # status print
  mt_print(verbose, "metnordic_csv", "finished! files have been written to:\n", paste0(directory, "/",foldername))
  # plot of cropped stations
  if(verbose){
    plot <- mapview::mapview(area, col.region = "orange")+mapview::mapview(cover_stations, cex = 3)
    print(plot)
  }

}
