#' Build MET Nordic Download Query
#'
#' This function builds the URL queries for downloading MET Nordic data through
#' the OPENDAP protocol. The requirements for this function to work are the
#' bounding coordinates as divined by function `metnordic_coordwindow()`, the
#' variables of interest, the starting and ending dates and the desired grid
#' resolution. The results of this function can be downloaded when passed to
#' `metnordic_download()`.
#'
#' @seealso [metnordic_coordwindow()] [metnordic_download()]
#' @author Moritz Shore
#'
#' @param bounding_coords as determined by `metnordic_coordwindow()`
#' @param mn_variables MET Nordic variables (see [documentation](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters))
#' @param fromdate ie. "2019-01-01 00:00:00"
#' @param todate ie. "2020-12-31 23:00:00"
#' @param grid_resolution an integer, ie. 3 for 3x3 km grid.
#' @param verbose print to console?
#'
#' @returns character vector of all the OPENDAP URLs to download.
#' @export
#'
#' @examples
#' # TODO
metnordic_buildquery <- function(bounding_coords, mn_variables, fromdate, todate,
                        grid_resolution, verbose){

  # time step not really needed since the files are individual
  time1 = 0
  time2 = 0
  timestep = 1
  # do point routine if true:
  if(length(bounding_coords) == 2){
    x = bounding_coords$index_x
    y = bounding_coords$index_y
    x1 = x
    x2 = x
    y1 = y
    y2 = y
    xstep = 1
    ystep = 1
    # if 4, do polygon routine
  }else if(length(bounding_coords) == 4){

    # unpacking the list pack
    index_xmin = bounding_coords$index_xmin
    index_xmax = bounding_coords$index_xmax
    index_ymin = bounding_coords$index_ymin
    index_ymax = bounding_coords$index_ymax

    # checking if the grid resolution is small enough to at least download 1
    # station.
    bbox_width = index_xmax - index_xmin
    bbox_height = index_ymax - index_ymin
    if(verbose){cat(green(italic("you have a grid of ", black(bold(bbox_width)), "x", black(bold(bbox_height)),"..",bbox_width*bbox_height, "cells\n")))}
    if(bbox_width < (2*grid_resolution)-1){stop("Area is not big enough (too narrow) for the given grid resolution. Please use a finer resolution")}
    if(bbox_height < (2*grid_resolution)-1){stop("Area is not big enough (too short) for the given grid resolution. Please use a finer resolution")}

    # from min x/y to max x/y by step of 1
    x1 = index_xmin
    x2 = index_xmax
    xstep = grid_resolution

    y1 = index_ymin
    y2 = index_ymax
    ystep = grid_resolution

    # print grid resolution
    if(verbose){cat(green(italic("generating urls with grid size of", black(bold(xstep)), "x", black(bold(ystep)), "km \n")))}
  }else{stop("bounding coords must be either 2 coordiates (point) or 4 (rectangle), you passed:", length(bounding_coords))}

  # paste together the vars
  x_q <- paste0("[", x1, ":", xstep,":", x2, "]")
  y_q <- paste0("[", y1, ":", ystep,":", y2, "]")
  time_q <- paste0("[",time1, ":", timestep,":", time2, "]")

  latitude <- paste0("latitude", y_q, x_q)
  longitude <-  paste0("longitude", y_q, x_q)
  altitude <- paste0("altitude", y_q, x_q)

  # paste together the variable query
  var_q <- paste0(mn_variables, time_q, y_q, x_q, collapse = ",")

  # paste together the full variable query
  var_query <-
    paste0("x",
           x_q,
           ",",
           "y",
           y_q,
           ",",
           latitude,
           ",",
           longitude,
           ",",
           altitude,
           ",",
           var_q)

  # create the daterange
  fromdate_lubed <- lubridate::as_datetime(fromdate)
  todate_lubed <- lubridate::as_datetime(todate)

  if(dataset == "reanalysis"){
    if(as_datetime(todate) > as_datetime("2023-01-31 23:00:00")){
      stop("Reanalysis 3 is only until '2023-01-31 23:00:00', contact maintainer if this changes..")
    }
    if(as_datetime(fromdate) < as_datetime("2012-09-01 03:00:00")){
      stop("Reanalysis 3 only starts '2012-09-01 03:00:00', contact maintainer if this changes..")
    }
  }else if (dataset == "operational"){
    if(as_datetime(todate) > as_datetime( Sys.time())){
      warning("`todate` lies in the future... the operational forecast might not exist for this time yet???")
    }
    #      met_analysis_1_0km_nordic_20180219T08Z.nc
    if(as_datetime(fromdate) < as_datetime("2018-02-19 08:00:00")){
      stop("Reanalysis 3 only starts '2018-02-19 08:00:00', contact maintainer if this changes.. (or use the re-analysis time for this")
    }
  }else if(dataset == 'continuous'){
    if(as_datetime(todate) > as_datetime( Sys.time())){
      warning("`todate` lies in the future... the operational forecast might not exist for this time yet???")
    }
    if(as_datetime(fromdate) < as_datetime("2012-09-01 03:00:00")){
      stop("Reanalysis 3 only starts '2012-09-01 03:00:00', contact maintainer if this changes..")
    }
  }else{
    stop(paste0("dataset = '", dataset, "' not supported or not recognized. Please pass either dataset = 'continuous', reanaysis' or 'operational'"))
  }

  daterange <- seq(fromdate_lubed,todate_lubed , by="hour")
  years <- lubridate::year(daterange)
  months <- lubridate::month(daterange) %>% stringr::str_pad(width = 2, side = "left", pad = "0")
  days <- lubridate::day(daterange) %>% stringr::str_pad(width = 2, side = "left", pad = "0")
  hours <- lubridate::hour(daterange) %>% stringr::str_pad(width = 2, side = "left", pad = "0")

  # create the file names using the date range
  filenames <- paste0("met_analysis_1_0km_nordic_", years, months, days, "T", hours, "Z", ".nc")
  # create the thredds filepath using the date range
  filepath <- paste0(years, "/", months,"/",days,"/")

  # header for thredds server
  header = "https://thredds.met.no/thredds/dodsC/metpparchivev3/"

  # full query URL pasted together
  full_urls <- paste0(header, filepath, filenames, "?", var_query)

  return(list(full_urls = full_urls, filenames = filenames))
}
