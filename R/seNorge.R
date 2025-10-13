#' Build SeNorge2018 Nordic Download Query
#'
#' This function builds the URL queries for downloading SeNorge data through
#' the OPENDAP protocol. The requirements for this function to work are the
#' bounding coordinates as divined by function `metnordic_coordwindow()`, the
#' variables of interest, the starting and ending dates and the desired grid
#' resolution. The results of this function can be downloaded when passed to
#' `senorge_download()`.
#'
#' @seealso [metnordic_coordwindow()] [senorge_download()]
#'
#' @param bounding_coords list as returned by `metnordic_coordwindow()`
#' @param variables character vector of variables to download may include the following: "tn", "tx", "rr", "tg"
#' @param fromdate date from when to start downloading data from (eg. "1957-01-01")
#' @param todate date from when to end downloading data from (eg. "2024-12-31")
#' @param grid_resolution integer value fo grid resolution (eg 1 for a 1x1km grid)
#' @param verbose logical, print?
#'
#' @importFrom lubridate ymd year yday leap_year
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom tidyr unite
#'
#' @returns Returns a named list with the urls and filenames which is to be passed to `senorge_download()`.
#' @export
#'
#' @examples
#' # TODO
senorge_buildquery <- function(bounding_coords, variables, fromdate, todate,
                                 grid_resolution = 1, verbose = FALSE){
  # Source:
  # https://thredds.met.no/thredds/catalog/senorge/seNorge_2018/Archive/catalog.html

  ## Check variable format:
  senorge_variables <- c("tn", "tx", "rr", "tg")
  if ((variables %in% senorge_variables) %>% all() == FALSE) {
    stop(
      "Provided variables not all in SeNorge2018. You can only request the following:\n >> ",
      paste(collapse = ", ", senorge_variables)
    )
  }

  ## Check start and end dates
  if(fromdate < ("1957-01-01" %>% as.Date())){stop("SeNorge only goes back to 1957. You requested data from: ", fromdate)}
  # Don't know how to check start date yet.


  ## Creating the suffix for the time dimension
  ## For SeNorge
  # https://thredds.met.no/thredds/dodsC/senorge/seNorge_2018/Archive/seNorge2018_1980.nc.html
  # hour of year
  # the unit say its hour since 1970 etc. But in reality I think the unit is days
  # hoy <- function(datetime){(datetime %>% lubridate::day()-1)*24 + datetime %>% lubridate::hour()}
  # time1 = hoy(fromdate)
  # time2 = hoy(todate)
  fromdate_c <- fromdate %>% as.Date()
  todate_c <- todate %>% as.Date()
  daterange <- seq(from = fromdate %>% lubridate::ymd(),
                   to = todate %>% lubridate::ymd(), by = "day")
  years <- daterange %>% lubridate::year() %>% unique()
  queryDF <- tibble(years)
  queryDF$time_q = lapply(years, year_to_query) %>% unlist()


  ## Building the geographic queries
  geo_queries <- build_coord_suffix(
    bounding_coords = bounding_coords,
    grid_resolution = grid_resolution,
    project = "senorge"
  )
  queryDF$x_q <- geo_queries$x_query
  queryDF$y_q <- geo_queries$y_query
  queryDF$X <- paste0("X", queryDF$x_q)
  queryDF$Y <- paste0("Y", queryDF$y_q)
  queryDF$latitude <- paste0("latitude", queryDF$y_q,queryDF$x_q)
  queryDF$longitude <- paste0("longitude", queryDF$y_q,queryDF$x_q)
  queryDF$time <- paste0("time",queryDF$time_q)

  # Generating variable queries
  var_q_gen <- function(variable){
    queryDF %>% mutate(variable = paste0(variable, time_q, y_q, x_q)) %>% pull(variable) %>% return()
  }
  lapply(variables, var_q_gen) %>% do.call("cbind", args = .) %>% as.data.frame() %>% as_tibble()-> var_qs
  colnames(var_qs) <- variables
  queryDF <- cbind(queryDF, var_qs) %>% as_tibble()

  # Add projection to query
  queryDF$proj <- "UTM_Zone_33"

  # add URL to query
  queryDF$url <- paste0("https://thredds.met.no/thredds/dodsC/senorge/seNorge_2018/Archive/seNorge2018_", queryDF$years, ".nc?")


  ### BUILD REAL QUERIES
  # f.ex:
  # https://thredds.met.no/thredds/dodsC/senorge/seNorge_2018/Archive/seNorge2018_
  # 1980.nc?X[0:1:1194],Y[0:1:1549],time[0:1:365],tg[0:1:0][0:1:0][0:1:0],
  # UTM_Zone_33,longitude[0:1:0][0:1:0],latitude[0:1:0][0:1:0],
  # rr[0:1:0][0:1:0][0:1:0],tx[0:1:0][0:1:0][0:1:0],tn[0:1:0][0:1:0][0:1:0]

  query_suffix <- queryDF %>% tidyr::unite(real_q, X, Y, time, proj, longitude, latitude, all_of(variables), sep = ",") %>% pull(real_q)
  queryDF$q_suff <- query_suffix
  full_queries <- paste0(queryDF$url, queryDF$q_suff)
  full_filenames <- paste0("seNorge2018_", years, ".nc")

  mt_print(verbose, "senorge_buildquery", "Returning queries..", paste0("(", length(full_filenames), ")"))
  return(list(full_urls = full_queries, filenames = full_filenames))
}

#' Download SeNorge2018 data
#'
#' This function downloads the files as queried by `metnordic_buildquery()`. If
#' you do not pass a `directory`, files will be downloaded in the working
#' directory. You have the option of downloading the following SeNorge2018
#' variables at daily resolution:
#' - `tn` (minimum daily temperature in Celsius)
#' - `tx` (maximum daily temperature in Celsius)
#' - `rr` (sum daily precipitation in mm),
#' - `tg` (mean daily temperature in Celsius)
#' If you do not provide a vector of variables to download, all will be downloaded.
#' You can also provide a `polygon` (Georeferenced simple feature, the same that
#' you provided to `metnordic_coordwindow()`) to verify that the download is
#' covering your desired area.
#' @param queries (list) generated by metnordic_buildqueries()
#' @param directory (char) folder where to download (will be created if it does not exist)
#' @param variables (char, vector) variables to download ("tn", "tx", "rr", "tg")
#' @param polygon (char) path to same polygon shapefile as passed to `metnordic_coordwindow()`
#' @param verbose (flag) print status?
#'
#' @returns path to downloaded files
#' @export
#'
senorge_download <- function(queries, directory = NULL, variables = NULL, polygon = NULL, verbose = FALSE){
  if(directory %>% is.null()){directory = getwd()}
  if(variables %>% is.null()){variables = c("tn", "tx", "rr", "tg")}
  dir.create(directory, recursive = T, showWarnings = F)
  list.files(directory, full.names = T) -> existing_files
  if(length(existing_files) > 0 ){
    file.remove(existing_files) -> rem_res
  }
  for (i in seq_along(queries$filenames)) {
    mt_print(verbose, "senorge_download", text = "Downloading:", queries$filenames[i])
    nc_filepath <- paste0(directory, "/", queries$filenames[i])
    ncin <- nc_open(queries$full_urls[i])
    all_vars <- ncin$var %>% names()
    nc_create(filename = nc_filepath, vars = ncin$var, force_v4 = F) -> newnc
    diagnostics = T
    for (variable in all_vars) {
      mt_print(verbose, "senorge_download", text = "Downloading..", variable, rflag = T)
      if(verbose){cat("                                                                               \r")}
      mt_print(diagnostics, "senorge_download", text = "Getting data for:", variable, rflag = T)
      # getting and applying values for current variable
      values <- ncvar_get(nc = ncin, varid = variable)
      ncvar_put(nc = newnc, varid = variable, vals = values)

      # adding each attribute
      mt_print(diagnostics, "senorge_download", text = "Applying attributes for", variable, rflag = T)
      attrs <- ncatt_get(nc = ncin, varid = variable)
      all_attrs <- attrs %>% names()
      for (attribute in all_attrs) {
        mt_print(diagnostics, "senorge_download", text = "Adding attribute:", paste0(variable, "[", attribute,"]"), rflag = T)
        attr_val <- attrs[[attribute]]
        ncatt_put(nc = newnc, varid = variable, attname = attribute, attval = attr_val)
        if(verbose){cat("\r")}
      }
      all_glob_attr <- ncatt_get(ncin, varid = 0)
      glob_attr_names <- all_glob_attr %>% names()
      for (glob_attr in glob_attr_names) {
        current_glob_attr <- all_glob_attr[[glob_attr]]
        mt_print(diagnostics, "senorge_download", text = "Applying global attribures:", text2 = glob_attr, rflag = T)
        ncatt_put(nc = newnc, varid = 0, attname = glob_attr, attval = current_glob_attr)

      }
    }
    nc_close(ncin)
    nc_close(newnc)
    mt_print(verbose, "senorge_download", text = "Finished Download                                                                   ",
             rflag = T)
    if(verbose){cat("\r\n")}
    # if a polygon simple feature is provided, then preview.
    if(polygon %>% is.null() == FALSE){
      terra::rast(nc_filepath, variables[1])[[1]] -> myrast
      title = paste(terra::longnames(x =myrast))
      subtitle = paste0(terra::sources(myrast), "\n", terra::time(myrast) %>% as.Date())
      ggplot2::ggplot() +
        tidyterra::geom_spatraster(data = myrast, interpolate = F)+
        ggplot2::geom_sf(data = polygon, color = "red", fill = NA, size = 2)+
        ggplot2::theme_bw()+
        ggplot2::ggtitle(title, subtitle)+
        ggplot2::guides(fill=ggplot2::guide_legend(title=terra::units(myrast))) -> myggplot
      print(myggplot)
    }
  }
  return(directory)
}
#
#


# Testing parameter set
# if(FALSE){
#   directory <- "../mt-testing/senorge/download_indiv_test2"
#   outdir <- "../mt-testing/senorge/extract"
#   area = "../swat-cs10/model_data/input/shape/cs10_basin.shp" %>% sf::read_sf()
#   variables <- c("tg", "tx", "tn", "rr")
#   buffer = 1000
#   verbose = T
#   senorge_extract_grid(directory,outdir, area, variables, buffer, verbose)
# }



#' Extract data from SeNorge2018 files
#'
#' WIP
#'
#' @param directory WIP
#' @param outdir WIP
#' @param area WIP
#' @param variables WIP
#' @param buffer WIP
#' @param verbose WIP
#'
#' @returns WIP
#' @export
#'
#' @examples
#'
#' WIP
senorge_extract_grid <- function(directory,outdir, area, variables, buffer, verbose){
  directory %>% list.files(full.names = T) -> filenames # move this into second function
  get_overlapping_cells(
    directory = directory,
    variables = variables,
    area = area,
    buffer = buffer,
    verbose = verbose
  ) %>% senorge_nc_to_df(directory = directory,
                         filenames = filenames,
                         verbose = verbose) %>%
    write_senorge(outdir = outdir, verbose = verbose)
  mt_print(verbose, "senorge_extract_grid", "Finished, files can be found here:", outdir)
  return(outdir)
}

# Returns data in tidy format from a grid.
senorge_nc_to_df <- function(grid, directory, filenames, verbose) {
  grid$geometry %>% length() -> station_nr
  filenames %>% stringr::str_split("_", simplify = T) -> splitted
  splitted[,ncol(splitted)] %>% stringr::str_remove_all(".nc") -> years
  extract_senorge_variable <- function(variable){

    extract_senorge_year <- function(year){
      # Grab the right file
      filepath <- list.files(directory,
                             pattern = paste0("seNorge2018_", year, ".nc"),
                             full.names = T)
      if(length(filepath) == 0){
        stop("File `", paste0(directory, "/seNorge2018_", year, ".nc"), "` not found!")
      }
      varrast <- terra::rast(filepath, variable)
      terra::time(varrast) %>% as.Date() -> timelist
      grid <- grid %>% sf::st_transform(sf::st_crs(varrast))
      datamatrix <- terra::extract(varrast, grid, method = "simple", bind = T)
      cell_to_df <- function(cell){
        mt_print(verbose, "senorge_extract_grid", paste0("Extracting ", variable, " [", year, "]"), paste0("[", cell, "/", station_nr, "]" ), rflag = T)
        if(verbose){cat("                                                                 \r")}
        datamatrix[cell] %>% as.data.frame() %>% t() %>% as.data.frame(row.names = F) -> mydf
        return_df <- tibble::tibble(date = timelist, value = mydf$V1, grid_cell = cell, variable = variable)
        return(return_df)
      }
      lapply(c(1:station_nr),FUN =cell_to_df) %>% do.call(what = "rbind") -> variable_full
      return(variable_full)
    }

    lapply(X = years, FUN = extract_senorge_year) -> one_var_year_list
    one_var_year_list %>% do.call(what = "rbind", args = .) -> one_var_all_year_df
    return(one_var_all_year_df)
  }
  mt_print(verbose, "senorge_extract_grid", "Extracting data from the following year(s):", paste0(years, collapse = ", "))
  lapply(X = variables, FUN = extract_senorge_variable) -> list_all_vars
  list_all_vars %>% do.call(what = "rbind") -> megadf
}

# writes files
write_senorge <- function(megadf, outdir, verbose){
  cells <- megadf %>% pull(grid_cell) %>% unique() %>% sort()
  dir.create(outdir, showWarnings = F)
  warning("problem with leap years here, probably needs to be fixed in query buildr")
  for (cell in cells) {
    mt_print(verbose, "senorge_extract_grid", "Writing cells to file..", paste0("[", cell, "/",  length(cells), "]"), rflag = T)
    if(verbose){cat("                                                                 \r")}
    megadf %>% filter(grid_cell ==  cell) %>%
      rename(vstation = grid_cell) %>%
      pivot_wider(values_from = value, names_from = variable, id_cols = c(vstation, date)) -> vstatslice
    filename <- paste0(outdir, "/", "senorge_vstation_", cell, ".csv")
    readr::write_csv(x = vstatslice, file = filename)
  }
}

## buils time query for SeNorge
year_to_query <- function(c_year){
  # we are operating on a daily fashion
  timestep = 1
  # if the current year is also the year of the start date, then grab
  # the DOY of the start date and set it to t1
  if(c_year == year(fromdate)){
    t1 <- yday(fromdate)-1
  }else{
    # if that is not the case, then we are starting at the first day of the
    # year I.e. 0
    t1 <- 0
  }
  # if the current year also matches that of the end date year, then set t2
  # to the DOY of the end date
  if(c_year == year(todate)){
    t2 <- yday(todate)-1
  }else if(c_year %>% leap_year()){
    # otherwise, set it to 365 (if its a leap year)
    t2 <-  365
  }else{
    # or 364 if it is not a leap year
    t2 <- 364
  }
  time_q <- paste0("[",t1, ":", timestep,":", t2, "]")
}
