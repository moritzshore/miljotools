#' Build MET Nordic Download Query
#'
#' This function builds the URL queries for downloading MET Nordic data through
#' the OPENDAP protocol. The requirements for this function to work are the
#' bounding coordinates as divined by function `metnordic_coordwindow()`, the
#' variables of interest, the starting and ending dates and the desired grid
#' resolution. The results of this function can be downloaded when passed to
#' `metnordic_download()`.
#'
#' @seealso [metnordic_coordwindow()] [metnordic_download()] [metnordic_download_daterange()]
#' @author Moritz Shore
#'
#' @param bounding_coords as passed by `metnordic_coordwindow()`
#' @param mn_variables MET Nordic variables (see [documentation](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters))
#' @param fromdate ie. "2019-01-01 00:00:00"
#' @param todate ie. "2020-12-31 23:00:00"
#' @param dataset either 'reanalysis' for the re-run archive, 'operational' for
#'   the operational archive, 'ltc' for the EXPERIMENTAL long term consistent product or 'continuous' to source from all, depending on
#'   date range.
#' @param grid_resolution an integer, ie. 3 for 3x3 km grid.
#' @param verbose print to console?
#'
#' @returns character vector of all the OPENDAP URLs to download.
#' @export
#' @importFrom lubridate hour day year
#'
#' @examples
#' # TODO
metnordic_buildquery <- function(bounding_coords, mn_variables, fromdate, todate,
                        grid_resolution = 1, dataset = 'reanalysis', verbose = FALSE){

  # detecting geometry by list structure
  if(length(bounding_coords) == 5){
    q_poly_geom <- (bounding_coords %>% names() == c("index_xmin", "index_xmax", "index_ymin", "index_ymax", "metadist")) %>% all()
    q_point_geom <- FALSE
  }else if(length(bounding_coords) == 3){
    q_point_geom <- (bounding_coords %>% names() == c("index_x", "index_y", "metadist")) %>% all()
    q_poly_geom <- FALSE
  }else{stop("bounding coords not recognized. please generate with 'metnordic_coordwindow()'")}

  # if the passed bounding coords match the polygon geomtry, remove the metadist
  if(q_poly_geom){
    bounding_coords <- bounding_coords[-5]
  }

  # time step not really needed since the files are individual
  time1 = 0
  time2 = 0
  timestep = 1
  # do point routine if true:
  if(q_point_geom){
    x = bounding_coords$index_x
    y = bounding_coords$index_y
    x1 = x
    x2 = x
    y1 = y
    y2 = y
    xstep = 1
    ystep = 1
    # or do polygon routine
  }else if(q_poly_geom){

    # unpacking the list pack
    index_xmin = bounding_coords$index_xmin
    index_xmax = bounding_coords$index_xmax
    index_ymin = bounding_coords$index_ymin
    index_ymax = bounding_coords$index_ymax

    # checking if the grid resolution is small enough to at least download 1
    # station.
    bbox_width = index_xmax - index_xmin
    bbox_height = index_ymax - index_ymin
    mt_print(verbose, "metnordic_buildquery", "You have a grid of:", paste0(bbox_width, " x ", bbox_height, " (", bbox_width*bbox_height, " cells)"))
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
    mt_print(verbose, "metnordic_buildquery", "generating urls with a grid size of:", paste0(xstep, " x ", ystep, " km"))
  }else{stop("bounding coords not recognized. please generate with 'metnordic_coordwindow()'")}

  ## paste together the vars
  # Subtract 1 from the x and y because OPDENDAP starts at 0 but R starts at 1
  x_q <- paste0("[", x1-1, ":", xstep,":", x2-1, "]")
  y_q <- paste0("[", y1-1, ":", ystep,":", y2-1, "]")
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
    if(lubridate::as_datetime(todate) > lubridate::as_datetime("2023-01-31 23:00:00")){
      stop("Reanalysis 3 is only until '2023-01-31 23:00:00', contact maintainer if this changes..")
    }
    if(lubridate::as_datetime(fromdate) < lubridate::as_datetime("2012-09-01 03:00:00")){
      stop("Reanalysis 3 only starts '2012-09-01 03:00:00', contact maintainer if this changes..")
    }
  }else if (dataset == "operational"){
    if(lubridate::as_datetime(todate) > lubridate::as_datetime( Sys.time())){
      warning("`todate` lies in the future... the operational forecast might not exist for this time yet???")
    }
    #      met_analysis_1_0km_nordic_20180219T08Z.nc
    if(lubridate::as_datetime(fromdate) < lubridate::as_datetime("2018-02-19 08:00:00")){
      stop("Reanalysis 3 only starts '2018-02-19 08:00:00', contact maintainer if this changes.. (or use the re-analysis time for this")
    }
  }else if(dataset == 'continuous'){
    if(lubridate::as_datetime(todate) > lubridate::as_datetime( Sys.time())){
      warning("`todate` lies in the future... the operational forecast might not exist for this time yet???")
    }
    if(lubridate::as_datetime(fromdate) < lubridate::as_datetime("1958-08-02 00:00:00")){
       warning("Dataset 'longterm consistent' (ltc) only starts @ '1958-08-02 00:00:00'. Trying to download these queries may result in an error!")
    }
  }else if (dataset == "ltc"){
    if(lubridate::as_datetime(todate) >= lubridate::as_datetime("2012-09-01 03:00:00")){
      warning("Your request for 'ltc' data includes timestamps which are covered by the (non-experimental) reanalysis product. Perhaps you should set the `dataset` parameter to 'continuous'?")
    }
    if(lubridate::as_datetime(fromdate) < lubridate::as_datetime("1958-08-02 00:00:00")){
       warning("Dataset 'longterm consistent' (ltc) only starts @ '1958-08-02 00:00:00'. Trying to download these queries may result in an error!")
    }
    }else{
      stop(
        paste0(
          "dataset = '",
          dataset,
          "' not supported or not recognized. Please pass either dataset = 'continuous', reanaysis', 'operational', or 'ltc'"
        )
      )
    }

  daterange <- seq(fromdate_lubed,todate_lubed , by="hour")
  years <- lubridate::year(daterange)
  months <- lubridate::month(daterange) %>% stringr::str_pad(width = 2, side = "left", pad = "0")
  days <- lubridate::day(daterange) %>% stringr::str_pad(width = 2, side = "left", pad = "0")
  hours <- lubridate::hour(daterange) %>% stringr::str_pad(width = 2, side = "left", pad = "0")

  # header for thredds server
  file_header_ltc <- "met_analysis_ltc_1_0km_nordic_"
  file_header_nordic <- "met_analysis_1_0km_nordic_"

  if(dataset == "ltc"){
    filenames <- paste0(file_header_ltc, years, months, days, "T", hours, "Z", ".nc")
  }else if(dataset %in% c("reanalysis", "operational")){
    filenames <- paste0(file_header_nordic, years, months, days, "T", hours, "Z", ".nc")
  }else if(dataset == "continuous"){
    filenames_ltc <- paste0(file_header_ltc, years, months, days, "T", hours, "Z", ".nc")
    filenames_mn <- paste0(file_header_nordic, years, months, days, "T", hours, "Z", ".nc")
    ltc_header_split <-  (lubridate::as_datetime(daterange) < lubridate::as_datetime("2012-09-01 03:00:00")) %>% which()
    mn_header_split <-  (lubridate::as_datetime(daterange) >= lubridate::as_datetime("2012-09-01 03:00:00")) %>% which()
    filenames <- c(filenames_ltc[ltc_header_split],filenames_mn[mn_header_split])
  }else{
    stop("dataset not understood..")
  }

  filepath <- paste0(years, "/", months,"/",days,"/")
  re_header = "https://thredds.met.no/thredds/dodsC/metpparchivev3/"
  op_header =  "https://thredds.met.no/thredds/dodsC/metpparchive/"
  ltc_header =  "https://thredds.met.no/thredds/dodsC/metppltcarchivev1/"
  if(dataset == "reanalysis"){
    full_urls <- paste0(re_header, filepath, filenames, "?", var_query)
  }else if(dataset == "operational"){
    full_urls <- paste0(op_header, filepath, filenames, "?", var_query)
  }else if(dataset == "ltc"){
    full_urls <- paste0(ltc_header, filepath, filenames, "?", var_query)
    }else if(dataset == "continuous"){
      # separated based off cut-off.
      ## WARNING this will need to change if metnordic ever updates the re-run.
      ## could replace with an optional parameter to have users define the cutoff..

      ltc_split <-  (lubridate::as_datetime(daterange) < lubridate::as_datetime("2012-09-01 03:00:00")) %>% which()
      reanal_split <- ((lubridate::as_datetime(daterange) <= lubridate::as_datetime("2023-01-31 23:00:00")) &
        (lubridate::as_datetime(daterange) >= lubridate::as_datetime("2012-09-01 03:00:00"))) %>% which()
      op_split <- (lubridate::as_datetime(daterange) > lubridate::as_datetime("2023-01-31 23:00:00")) %>% which()

    if(length(ltc_split) > 0){
      ltc_full_urls <-  paste0(ltc_header, filepath[ltc_split], filenames[ltc_split], "?", var_query)
    }else{
      ltc_full_urls <- c()
    }

    if(length(reanal_split)>0){
      re_full_urls <- paste0(re_header, filepath[reanal_split], filenames[reanal_split], "?", var_query)
    }else{
      re_full_urls <- c()
    }

    if(length(op_split)>0){
      op_full_urls <- paste0(op_header, filepath[op_split], filenames[op_split], "?", var_query)
    }else{
      op_full_urls <- c()
    }
    full_urls <- c(ltc_full_urls, re_full_urls, op_full_urls)
  }else{
    stop("parameter `dataset` not recognized!")
  }

  return(list(full_urls = full_urls, filenames = filenames))
}
