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
  # TEMP SITUATION: Senorge only per year downloads, so update the to and from
  # date. This should be improved to go per day someday
  if(dataset == "senorge" & nchar(todate) == 4){
    todate <- paste0(todate, "-01-01")
    fromdate <- paste0(fromdate, "-12-31")
  }

  # detecting geometry by list structure
  if(length(bounding_coords) == 5){
    q_poly_geom <- (bounding_coords %>% names() == c("index_xmin", "index_xmax", "index_ymin", "index_ymax", "metadist")) %>% all()
    q_point_geom <- FALSE
  }else if(length(bounding_coords) == 3){
    q_point_geom <- (bounding_coords %>% names() == c("index_x", "index_y", "metadist")) %>% all()
    q_poly_geom <- FALSE
  }else{stop("bounding coords not recognized. please generate with 'metnordic_coordwindow()'")}

  # if the passed bounding coords match the polygon geometry, remove the metadist
  if(q_poly_geom){
    bounding_coords <- bounding_coords[-5]
  }

  if(dataset == "senorge"){
    ## For SeNorge
    # https://thredds.met.no/thredds/dodsC/senorge/seNorge_2018/Archive/seNorge2018_1980.nc.html
    # hour of year
    # the unit say its hour since 1970 etc. But in reality I think the unit is days
    # hoy <- function(datetime){(datetime %>% lubridate::day()-1)*24 + datetime %>% lubridate::hour()}
    # time1 = hoy(fromdate)
    # time2 = hoy(todate)
    time1 = lubridate::yday(fromdate)
    time2 = lubridate::yday(todate)
    time1 = 0
    time2 = 364
    timestep = 1
  }else if(dataset %in% c("metnordic", "operational", "continuous")){
    ## For MetNordic
    # time step not really needed since the files are individual
    time1 = 0
    time2 = 0
    timestep = 1
  }else{
    stop("dataset parameter invalid!")
  }

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

    # this happens with the SeNorge data, no idea why.
    if(index_ymax < index_ymin){
      temp <- index_ymax
      index_ymax <- index_ymin
      index_ymin <- temp
    }

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
  if (dataset == "senorge") {
    senorge_variables <- c("tn", "tx", "rr", "tg")
    if ((mn_variables %in% senorge_variables) %>% all() == FALSE) {
      stop(
        "Provided variables not all in SeNorge2018. You can only request the following:\n >> ",
        paste(collapse = ", ", senorge_variables)
      )
    }

    }
  var_q <- paste0(mn_variables, time_q, y_q, x_q, collapse = ",")

  # paste together the full variable query
  # Senorge uses big X and Y instead of small x
  # Senorge does not have altitude.
  if (dataset == "senorge") {
    var_query <- paste0("X",
                        x_q,
                        ",",
                        "Y",
                        y_q,
                        ",",
                        "time", time_q, ",",
                        "UTM_Zone_33,",
                        latitude,
                        ",",
                        longitude,
                        ",",
                        var_q)
  }else{
    # Met nordic uses small x and y
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
  }


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
  }else if(dataset == "senorge"){
    if(lubridate::as_datetime(todate) > lubridate::as_datetime( Sys.time())){
      warning("`todate` lies in the future... SeNorge might not exist for this time yet???")
    }

    if(lubridate::as_datetime(fromdate) < lubridate::as_datetime("1957-01-01")){
      warning("`fromdate` is before 1957... SeNorge does not go that far back. (or has something changed? contact maintainer)")
    }
  }else{
    stop(paste0("dataset = '", dataset, "' not supported or not recognized. Please pass either dataset = 'continuous', reanaysis' or 'operational'"))
  }

  if(dataset == "senorge"){
    # https://thredds.met.no/thredds/dodsC/senorge/seNorge_2018/Archive/seNorge2018_1957.nc
    daterange <- seq(fromdate_lubed,todate_lubed , by="day")
    years = lubridate::year(daterange) %>% unique()
    filenames <- paste0("seNorge2018_", years, ".nc")
    header = "https://thredds.met.no/thredds/dodsC/senorge/seNorge_2018/Archive/"
    full_urls <- paste0(header, filenames, "?", var_query)


    # Failed attempt at replacing the time query with sub-year ones.
    # lubridate::yday(todate) -> start_day
    # lubridate::yday(fromdate) -> end_day
    #
    # # if only within 1 year is being downloaded..
    # if(lubridate::year(todate) == lubridate::year(fromdate)){
    #   replace_time_dim_per_var <- function(variable){
    #     new_time_q = paste0("[", start_day, ":1:", end_day,"]")
    #     old_string = paste0(variable, time_q)
    #     new_string = paste0(variable, new_time_q)
    #     stringr::str_replace_all(full_urls, pattern = old_string, replacement = new_string)
    #   }
    # }

    # https://thredds.met.no/thredds/dodsC/senorge/seNorge_2018/Archive/seNorge2018_1957.nc?X[0:1:1194],Y[0:1:1549],time[0:1:364],tg[0:1:0][0:1:0][0:1:0],UTM_Zone_33,longitude[0:1:0][0:1:0],latitude[0:1:0][0:1:0],nv[0:1:1],time_bnds[0:1:0][0:1:0],rr[0:1:0][0:1:0][0:1:0],tx[0:1:0][0:1:0][0:1:0],tn[0:1:0][0:1:0][0:1:0]
  }else if(dataset %in% c("operational", "reanalysis", "continuous")){
    # Method for met nordic
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
  }

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
  mt_print(verbose, "metnordic_buildquery", "Returning queries..", paste0("(", length(filenames), ")"))
  return(list(full_urls = full_urls, filenames = filenames))
}
