#' Merge MET Nordic files (hourly)
#'
#' This function lets you combine the hourly files downloaded by
#' `download_metnordic()` into single files. **Note**, these files are separate
#' per variable!
#'
#' @param folderpath (String) folder where individual files are located
#' @param variable (String) MET Nordic variable to combine
#' @param outpath (String) folder where to write the file
#' @param n_cores (Integer) Number of cores to use for parallel processing (Defaults to max - 2)
#' @param overwrite (Boolean) overwrite existing file?
#' @param verify (Boolean) an optional check to see if all files to be merged are incremental (Recommended!)
#' @param verbose (Boolean) print status?
#'
#' @importFrom parallel  detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#' @returns path of written file
#' @export
#'
#' @seealso [metnordic_download()] [metnordic_extract()]
metnordic_merge_hourly <- function(folderpath, variable, outpath, n_cores = NULL, overwrite = FALSE, verify = FALSE, verbose = FALSE) {
  mt_print(TRUE, "metnordic_merge_hourly", text = "Scanning files..")
  # Grabbing files
  short_fps <- list.files(folderpath, pattern = "*.nc")
  long_fps <- list.files(folderpath, pattern = "*.nc", full.names = T)
  short_fps_filt <- short_fps[(grepl(x = short_fps, pattern = variable) %>% which())]
  long_fps_filt <- long_fps[(grepl(x = short_fps, pattern = variable) %>% which())]
  templatenc <- ncdf4::nc_open(long_fps_filt[1])
  # opening the first nc file to serve as a template. many attributes are
  # extracted
  ### Elaborate NC file definition scheme: (mostly copied from `download_metnordic()`)
  lon <- ncdf4::ncvar_get(templatenc,"lon")
  lat <- ncdf4::ncvar_get(templatenc,"lat")
  alt <- ncdf4::ncvar_get(templatenc, "altitude")
  nlat <- dim(lat)
  nlon <- dim(lon)
  x <- ncdf4::ncvar_get(templatenc,"x")
  xlname <- ncdf4::ncatt_get(templatenc,"x","standard_name")
  xunits <- ncdf4::ncatt_get(templatenc,"x","units")
  nx <- dim(x)
  y <- ncdf4::ncvar_get(templatenc,"y")
  ylname <- ncdf4::ncatt_get(templatenc,"y","standard_name")
  yunits <- ncdf4::ncatt_get(templatenc,"y","units")
  ny <- dim(y)
  fillvalue <- 1e32

  # here we determine the correct format for the time dimension
  # "days since 1901-01-01 00:00:00.0"
  # note, UNSTABLE! dependent on file path ([,6] and [,1])
  headers <- c("met_analysis_ltc_1_0km_nordic_", "met_analysis_1_0km_nordic_")
  short_fps_filt %>% stringr::str_remove_all(headers[1]) %>%
    stringr::str_remove_all(headers[2]) %>%
    stringr::str_remove_all(paste0("_", variable)) %>%
    stringr::str_remove_all(".nc") %>%
    stringr::str_remove_all("Z") -> filenames_date
  # Converting the file names into date times.
  dateformat <-  paste0(
    substring(filenames_date, 1, 4),
    "-",
    substring(filenames_date, 5, 6),
    "-",
    substring(filenames_date, 7, 8),
    " ",
    substring(filenames_date, 10, 11),
    ":00:00"
  )

  # check to see if the conversion worked.
  if((dateformat[1] %>% lubridate::as_datetime() %>% is("POSIXct")) == FALSE){
    stop("Something went wrong converting the filenames to dates: \n", dateformat[1] )
  }

  dateformat <- dateformat %>% sort()

  if(verify){
    daterange <- dateformat %>% range()
    mt_print(verbose, "metnordic_merge_hourly", "verifiyng following timerange:", paste0(daterange[1], " - ", daterange[2]))
    seq(daterange[1] %>% lubridate::as_datetime(tz = "UTC"), daterange[2]%>% lubridate::as_datetime(tz = "UTC"), by = "hour") -> fulldate
    fulldate %>% strftime(tz = "UTC") -> full_date_notz
    suppressWarnings((dateformat != full_date_notz) %>% which()) -> issue_idx
    if(issue_idx %>% length() > 0){
      issue_idx = min(issue_idx)
      stop("[VERIFY = TRUE] Missing file detected at following date: ",   full_date_notz[issue_idx], " for '", variable, "'.
         \n >> File after '", short_fps_filt[issue_idx-1], "' is probably missing and needs to be redownloaded!
         \n >> The easiest way to fix this is to delete all files of datetime ", full_date_notz[issue_idx], " and re-run `metnordic_download_daterange()`")
    }else{
      mt_print(verbose, "metnordic_merge_hourly", "all files verified to be incremental..")
    }
  }

  # function to extract data from every file (used in parallel)
  vect_open_return_na <- function(timestamp) {
    filedate <- paste0(
      substr(timestamp, 0, 4),
      substr(timestamp, 6, 7),
      substr(timestamp, 9, 10),
      "T",
      substr(timestamp, 12, 13),
      "Z_", variable, ".nc"
    )
    filepath = list.files(folderpath, pattern = filedate, full.names = T)
    if(length(filepath) == 0){
      slice = matrix(data = NA, nrow = nx, ncol = ny)
    }else{
      ncfile <- ncdf4::nc_open(filepath)
      slice <- ncdf4::ncvar_get(ncfile, variable)
      ncdf4::nc_close(ncfile)
    }
    return(slice)
  }

  ## Merge in parallel
  if(is.null(n_cores)){
    n_cores = parallel::detectCores() - 2
  }
  mt_print(verbose, "metnordic_merge_hourly", paste0("Merging ", length(dateformat), " files: "), paste0(variable, " (using ", n_cores, " threads)"))
  cl <-parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  result <- foreach(hour = dateformat) %dopar% {vect_open_return_na(timestamp = hour)}
  parallel::stopCluster(cl)

  # if this is the case, a single point NC file has been downloaded.
  if(result[[1]] %>% dim() %>% length() == 1){
   stop("sorry, this function does not support merging NC files from a point geometry (1D). Please consider downloading from a polygon area (2D) or try using `metnordic_csv()")
  }else if(result[[1]] %>% dim() %>% length() == 2){
    # if this is the case, a 2D area has been downloaded.
    datacube <- result %>% abind::abind(along = 3)
  }else{
    stop("Format not recognized.")
  }

  # create the file path to be written, check if it already exists. if the
  # overwrite flag is enabled, then the existing file is deleted to be created
  # again. if not, the function skips this file.
  daterange <- paste0(filenames_date[1],"-", filenames_date[length(filenames_date)])
  write_fp <- paste0("metno-", variable, "_", daterange, ".nc")
  full_write_fp <- paste0(outpath, "/", write_fp)
  dir.create(outpath, showWarnings = F)
  if(file.exists(full_write_fp)){
    if(overwrite){
      warning("file already exists, overwriting: ", write_fp, "\n")
      removed = file.remove(full_write_fp)
      if(removed == FALSE){stop("error with removing file",full_write_fp )}
    }else{
      warning(" '",write_fp, "' already exists, skipping..")
      return(full_write_fp)
    }
  }

  ### Define
  ## X and Y dimensions
  xdim <- ncdf4::ncdim_def("x",units="m",
                    longname="eastward distance from southwest corner of domain in projection coordinates",as.double(x))
  ydim <- ncdf4::ncdim_def("y",units="m",
                    longname="northward distance from southwest corner of domain in projection coordinates",as.double(y))
  ## Time dimensions:
  tunits <- "hours since 1901-01-01 01:00:00"
  source_date = "1901-01-01 01:00:00"
  first_date = dateformat[1]
  basehour <- difftime(first_date, source_date, units =  "hour") %>% as.numeric() # I confirmed this works using timeanddate.com
  post_hours <- c(0:(length(dateformat)-1))
  hours_since_1901_01 <-basehour+post_hours # the first hour should remain unchanged, therefore start @ 0 and length-1
  timedim <- ncdf4::ncdim_def(name = "time" ,units = tunits,vals = hours_since_1901_01, unlim = F, calendar = "proleptic_gregorian")

  # Spatial Definitions
  lon_def  <- ncdf4::ncvar_def(name = "lon",units = "degrees_east",dim = list(xdim,ydim),NULL, longname = "Longitude of cell center",prec="double")
  lat_def  <- ncdf4::ncvar_def(name = "lat",units = "degrees_north",dim = list(xdim,ydim),missval = NULL, longname = "Latitude of cell center",prec="double")
  proj_def <- ncdf4::ncvar_def(name = "lambert_conformal_conic",units = "1",dim = NULL,missval = NULL, prec="char")
  alt_def  <- ncdf4::ncvar_def(name = "altitude",units = "m",dim = list(xdim,ydim),missval = NULL, prec="integer")

  # Variable definition
  dunits <- ncdf4::ncatt_get(templatenc,variable,"units")
  current_var_def <-  ncdf4::ncvar_def(variable,dunits$value,list(xdim,ydim,timedim),fillvalue,variable,prec="double")
  ncdf4::nc_close(templatenc)

  ### Creating the new file
  ncout <-  ncdf4::nc_create(full_write_fp,list(current_var_def,alt_def, lon_def,lat_def,proj_def),force_v4=TRUE)

  # Adding the data
  ncdf4::ncvar_put(ncout,current_var_def,datacube)
  ncdf4::ncvar_put(ncout,lon_def,lon)
  ncdf4::ncvar_put(ncout,lat_def,lat)
  ncdf4::ncvar_put(ncout, alt_def, alt)

  # Add additional attributes into dimension and data variables
  ncdf4::ncatt_put(ncout,"x","axis","X")
  ncdf4::ncatt_put(ncout,"x","standard_name","projection_x_coordinate")
  ncdf4::ncatt_put(ncout,"x","_CoordinateAxisType","GeoX")
  ncdf4::ncatt_put(ncout,"y","axis","Y")
  ncdf4::ncatt_put(ncout,"y","standard_name","projection_y_coordinate")
  ncdf4::ncatt_put(ncout,"y","_CoordinateAxisType","GeoY")
  ncdf4::ncatt_put(ncout, variable,"grid_mapping", "lambert_conformal_conic")
  ncdf4::ncatt_put(ncout, variable,"coordinates", "lat lon")

  # Add the CRS attributes
  projname <- "lambert_conformal_conic"
  longitude_of_central_meridian = 15
  latitude_of_projection_origin <- 63
  earth_radius <- 6371000
  standard_parallel <- 63
  false_easting <- 0
  false_northing <- 0
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","name",projname)
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","long_name",projname)
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","grid_mapping_name",projname)
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","longitude_of_central_meridian", as.double(longitude_of_central_meridian))
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","latitude_of_projection_origin", as.double(latitude_of_projection_origin))
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","earth_radius", as.double(earth_radius))
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","standard_parallel", c(standard_parallel, standard_parallel))
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","false_easting",false_easting)
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","false_northing",false_northing)
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","_CoordinateTransformType","Projection")
  ncdf4::ncatt_put(ncout,"lambert_conformal_conic","_CoordinateAxisTypes","GeoX GeoY")

  # add global attributes (TODO: cleanup)
  ncdf4::ncatt_put(ncout,0,"title",paste0("MET Nordic dataset variable ",variable, "(hourly)"))
  ins_text <- paste0("Sourced from MetNordic ++ Downloaded and processed miljotools version ",utils::packageVersion("miljotools"), " (https://github.com/moritzshore/miljotools)")
  ncdf4::ncatt_put(ncout,0,"institution",ins_text)
  history <- paste("Creaed ", date())
  ncdf4::ncatt_put(ncout,0,"history",history)
  ncdf4::ncatt_put(ncout,0,"Package URL", "https://github.com/moritzshore/miljotools")
  ncdf4::ncatt_put(ncout,0,"Conventions","CF=1.6")
  ncdf4::ncatt_put(ncout,0,"source files",folderpath)

  # Close the file, writing data to disk
  ncdf4::nc_close(ncout)

  # Return the filepath
  return(full_write_fp)
}

