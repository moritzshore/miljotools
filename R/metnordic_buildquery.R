#' Build MET Nordic Download Query
#'
#' This function builds the URL queries for downloading MET Nordic / SeNorge data through
#' the OPENDAP protocol. The requirements for this function to work are the
#' bounding coordinates as divined by function `metnordic_coordwindow()`, the
#' variables of interest, the starting and ending dates and the desired grid
#' resolution. The results of this function can be downloaded when passed to
#' `metnordic_download()` or `senorge_download()`.
#'
#' @seealso [metnordic_coordwindow()] [metnordic_download()] [metnordic_download_daterange()] [senorge_download()]
#' @author Moritz Shore
#'
#' @param bounding_coords as passed by `metnordic_coordwindow()`
#' @param mn_variables variable names of MET Nordic  (see [MET Nordic documentation](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters)) or SeNorge2018 ("tn", "tx", "rr", "tg").
#' @param fromdate ie. "2019-01-01 00:00:00"
#' @param todate ie. "2020-12-31 23:00:00"
#' @param dataset either 'reanalysis' for the re-run archive, 'operational' for
#'   the operational archive 'continuous' to source from both, depending on
#'   time range. Setting this parameter to 'senorge' will build queries for the [SeNorge2018](https://thredds.met.no/thredds/catalog/senorge/seNorge_2018/Archive/catalog.html) dataset.
#' @param grid_resolution an integer, ie. 3 for 3x3 km grid.
#' @param verbose print to console?
#'
#' @returns Returns a list with  all the OPENDAP URLs to download as well as their filenames.
#' @export
#' @importFrom lubridate hour day year
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
    if(lubridate::as_datetime(fromdate) < lubridate::as_datetime("2012-09-01 03:00:00")){
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

  # header for thredds server
  filenames <- paste0("met_analysis_1_0km_nordic_", years, months, days, "T", hours, "Z", ".nc")
  filepath <- paste0(years, "/", months,"/",days,"/")

  if(dataset == "reanalysis"){
    header = "https://thredds.met.no/thredds/dodsC/metpparchivev3/"
    full_urls <- paste0(header, filepath, filenames, "?", var_query)
  }else if(dataset == "operational"){
    header = "https://thredds.met.no/thredds/dodsC/metpparchive/"
    full_urls <- paste0(header, filepath, filenames, "?", var_query)
  }else if(dataset == "continuous"){
    # seperated based off cut-off.
    re_header = "https://thredds.met.no/thredds/dodsC/metpparchivev3/"  # rerun = v3
    op_header =  "https://thredds.met.no/thredds/dodsC/metpparchive/"

    ## WARNING this will need to change if metnordic ever updates the re-run.
    ## could replace with an optional parameter to have users define the cutoff..
    reanal_split <- (lubridate::as_datetime(daterange) <= lubridate::as_datetime("2023-01-31 23:00:00")) %>% which()
    op_split <- (lubridate::as_datetime(daterange) > lubridate::as_datetime("2023-01-31 23:00:00")) %>% which()

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

    full_urls <- c(re_full_urls,op_full_urls)
  }else{
    stop("parameter `dataset` not recognized!")
  }

  return(list(full_urls = full_urls, filenames = filenames))
}
