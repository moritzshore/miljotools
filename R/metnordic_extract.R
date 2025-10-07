#mn_variables <- c("air_temperature_2m", "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time", "precipitation_amount", "relative_humidity_2m", "wind_speed_10m")

#' MET Nordic Extract - Point Timeseries regional downloads
#'
#' This function extracts timeseries from (hourly) files from the chain
#' `metnordic_coordwindow()` -->  `metnordic_buildquery()` -->
#' `metnordic_download_daterange()` --> `metnordic_merge_hourly()` -->
#' `metnordic_extract()`. This function extracts from the nearest grid cell, if
#' you would like a bi-linear interpolation of the nearest 4 cells, please try
#' `metnordic_extract_grid()`
#'
#' @importFrom lubridate as_datetime
#' @importFrom readr write_csv read_csv
#' @importFrom sf st_crs st_geometry st_transform st_coordinates st_as_sf st_distance st_union st_as_sfc st_cast st_geometry
#' @importFrom methods is
#' @importFrom tibble tibble
#' @importFrom dplyr  %>% select nth
#' @importFrom ncdf4 nc_open ncvar_get ncatt_get nc_close
#'
#' @param directory (String) directory containing merged files as created by `metnordic_merge_hourly()`
#' @param mn_variables (vector, strings) variables to extract
#' @param point an object of class "sf" with point geometry.
#' @param outdir (string) directory in which to write the file (.csv)
#' @param name (string) name of the file (will be added to filename)
#' @param verbose (boolean) print?
#'
#' @seealso [metnordic_extract_grid()] [metnordic_merge_hourly()] [swap_metnordic()]
#'
#' @returns path to written file
#' @export
#'
#'
metnordic_extract <-  function(directory, mn_variables, point, outdir, name, verbose = FALSE) {

  # input validation
  if(is(point, "sf") == FALSE){stop("point needs to be an object of class 'sf'!")}
  if((point$geometry %>% length()) > 1){stop("you can only pass a single point at a time!")}
  if((sf::st_geometry(point) %>% attr("class") %>% dplyr::nth(1)) != "sfc_POINT"){stop("point is not of correct geometry! needs to be of class 'sfc_POINT'")}


  # lambert conform conical (the projection used by met reanalysis)
  projection <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6371000"
  proj_crs <- sf::st_crs(projection) # replace with sf::crs()

  metadf = get_meta(directory = directory,
    mn_variables = mn_variables,
    point = point,
    proj_crs = proj_crs,
    name = name,
    verbose = verbose
  )

  extract_var <- function(infp, variable, point) {

    point_proj <- sf::st_transform(point, crs = proj_crs)
    coordinate <- sf::st_coordinates(point_proj)
    point_x <- coordinate[1]
    point_y <- coordinate[2]

    ## Finding the nearest neighbor to each corner
    # calculate the difference in value
    ncin <- ncdf4::nc_open(infp)
    x <- ncdf4::ncvar_get(ncin, "x")
    y <- ncdf4::ncvar_get(ncin, "y")

    x_diff <- abs(x-point_x)
    y_diff <- abs(y-point_y)

    # find the minimum
    min_diff_x <- min(x_diff)
    min_diff_y <- min(y_diff)

    # find the index of the minimum
    index_x <- which(min_diff_x == x_diff)
    index_y <- which(min_diff_y == y_diff)

    df <- sf::st_as_sf(x = data.frame(x = x[index_x], y = y[index_y]),
                       coords = c("x", "y"), crs = proj_crs)

    datenumeric <-  ncdf4::ncvar_get(ncin, varid = "time")
    # Then to convert hours to seconds which are the basis for the POSIXt
    # classed objects, just multiply by 3600 = 60*60:
    # https://stackoverflow.com/a/30783581
    datetime <- as.POSIXct(datenumeric*3600,origin='1901-01-01 00:00:00',) %>% lubridate::as_datetime() #%>% strftime() this causes issues with summer time, do not use!
    brick <- ncdf4::ncvar_get(ncin, varid = variable)
    timeseries <- brick[index_x, index_y, ]
    res <- tibble::tibble(date = datetime, variable = timeseries)
    colnames(res) <- c("date", variable)
    ncdf4::nc_close(ncin)
    return(res)
  }

  extract_all_vars <- function(the_var) {
    infp = list.files(directory, the_var, full.names = T)
    if(infp %>% length() == 0){
      stop(" '", the_var, "' does not exist in '", directory, "', did you forget to merge it?")
    }
    extract_var(infp = infp,
                point = point,
                variable =  the_var)
  }

  reslist <- lapply(X = mn_variables, FUN = extract_all_vars)

  datestart <-  reslist[[1]][[1]][1] %>% lubridate::as_datetime()
  dateend <-  reslist[[1]][[1]][length(reslist[[1]][[1]])] %>% lubridate::as_datetime()
  daterange <- seq(from = datestart, to = dateend, by = "hour")
  full_df <- tibble::tibble(date = daterange)

  # left joining in case of NAs
  for (i in c(1:length(reslist))) {
    dis <- reslist[[i]]
    full_df<-left_join(full_df, reslist[[i]], by = "date", relationship = "one-to-one")
  }
  writefp <- paste0(outdir, "/METNORDIC_point_", name, ".csv")
  metafp <- paste0(outdir, "/METNORDIC_meta_", name, ".csv")

  dir.create(outdir, showWarnings = F)
  readr::write_csv(x = full_df, file = writefp)
  paste(names(metadf), "=", metadf, collapse = "\n") %>% writeLines(con = metafp)

  return(writefp)
}

get_meta <- function(directory, name, mn_variables, point, proj_crs=NULL, verbose){
  infp = list.files(directory, mn_variables[1], full.names = T)
  if(length(infp) > 1){stop("multiple files of the same variable [", mn_variables[1],"] detected in '", directory, "' Only one is allowed! Make sure to merge individual files with `metnordic_merge_hourly()`")}
  projection <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6371000"
  point_proj <- sf::st_transform(point, crs = projection)
  coordinate <- sf::st_coordinates(point_proj)
  point_x <- coordinate[1]
  point_y <- coordinate[2]

  ## Finding the nearest neighbor to each corner
  # calculate the difference in value
  ncin <- ncdf4::nc_open(infp)
  x <- ncdf4::ncvar_get(ncin, "x")
  y <- ncdf4::ncvar_get(ncin, "y")
  alt <- ncdf4::ncvar_get(ncin, "altitude")

  # TODO Add altitude to download always!
  #alt <- ncdf4::ncvar_get(ncin, "altitude")
  meta_text <- ncdf4::ncatt_get(ncin,varid = 0, attname = "institution")
  meta_text <- meta_text$value
  ncdf4::nc_close(ncin)
  x_diff <- abs(x-point_x)
  y_diff <- abs(y-point_y)

  # find the minimum
  min_diff_x <- min(x_diff)
  min_diff_y <- min(y_diff)

  # find the index of the minimum
  index_x <- which(min_diff_x == x_diff)
  index_y <- which(min_diff_y == y_diff)

  df <- sf::st_as_sf(x = data.frame(x = x[index_x], y = y[index_y]),
                     coords = c("x", "y"),
                     crs = projection)
  metadist <- (sf::st_distance(df, point_proj) %>% round(0))[1,1]

  # getting elevation:
  projection <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6371000"
  proj_crs <- sf::st_crs(projection) # replace with sf::crs()
  point_proj <- sf::st_transform(point, crs = proj_crs)
  coordinate <- sf::st_coordinates(point_proj)
  point_x <- coordinate[1]
  point_y <- coordinate[2]
  elevation = alt[index_x, index_y]

  if(verbose){
    geom_line <- geometry <- NULL
    xy <- cbind(df %>% dplyr::select(geometry), point_proj %>% dplyr::select(geometry))
    sf::st_geometry(xy) <- "geom_line"
    map1 = mapview::mapview(point_proj %>% dplyr::select(geometry), col.region = "orange", label = "Provided Point", layer.name = "Provided Point")
    map2 = mapview::mapview(df %>% dplyr::select(geometry), col.region = "purple", label = "MET Nordic Grid Cell Center", layer.name = "MET Nordic Gridcell")
    map3 = mapview::mapview(xy %>% dplyr::select(geom_line), layer.name = paste0("Distance = ", metadist, " m"), color = "black", label = paste0(metadist, " m"))
    plot <- map1+map2+map3
    print(plot)
  }

  meta_tib <- tibble::tibble(
    name = name,
    variables = mn_variables %>% paste(collapse = ","),
    metno_x = x[index_x],
    metno_y = y[index_y],
    metno_altitude = elevation,
    # TODO need to add
    point_x = point_x,
    point_y = point_y,
    distance_to_gridcell = metadist,
    source_projection = projection,
    source = meta_text,
    date = Sys.time() %>% as.character()
  )

  return(meta_tib)
}
