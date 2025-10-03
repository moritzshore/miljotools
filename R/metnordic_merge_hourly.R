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
#'
#' @importFrom parallel  detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#' @returns path of written file
#' @export
#'
#' @seealso [metnordic_download()] [metnordic_extract()]
metnordic_merge_hourly <- function(folderpath, variable, outpath, n_cores = NULL, overwrite = FALSE) {
  short_fps <- list.files(folderpath, pattern = "*.nc")
  long_fps <- list.files(folderpath, pattern = "*.nc", full.names = T)
  short_fps_filt <- short_fps[(grepl(x = short_fps, pattern = variable) %>% which())]
  long_fps_filt <- long_fps[(grepl(x = short_fps, pattern = variable) %>% which())]

  templatenc <- nc_open(long_fps_filt[1])
  # opening the first nc file to serve as a template. many attributes are
  # extracted
  ### Elaborate NC file definition scheme: (mostly copied from `download_metnordic()`)
  lon <- ncvar_get(templatenc,"lon")
  lat <- ncvar_get(templatenc,"lat")
  nlat <- dim(lat)
  nlon <- dim(lon)
  x <- ncvar_get(templatenc,"x")
  xlname <- ncatt_get(templatenc,"x","standard_name")
  xunits <- ncatt_get(templatenc,"x","units")
  nx <- dim(x)
  y <- ncvar_get(templatenc,"y")
  ylname <- ncatt_get(templatenc,"y","standard_name")
  yunits <- ncatt_get(templatenc,"y","units")
  ny <- dim(y)
  fillvalue <- 1e32

  # here we determine the correct format for the time dimension
  # "days since 1901-01-01 00:00:00.0"
  # note, UNSTABLE! dependent on file path ([,6] and [,1])
  filenames_date <- ((str_split(short_fps_filt, pattern = "_", simplify = T)[,6]) %>% str_split(pattern = "-", simplify = T))[,1]

  # WARNING I think you should remove strftime! --> I checked the summer time, seems to work. but no october in the range i tested
  dateformat <-  paste0(
    substring(filenames_date, 1, 4),
    "-",
    substring(filenames_date, 5, 6),
    "-",
    substring(filenames_date, 7, 8),
    " ",
    substring(filenames_date, 10, 11),
    ":00:00"
  ) %>% as_datetime() %>% strftime(tz = "UTC")


  full_date_range <- seq(dateformat[1] %>% as_datetime(), dateformat[length(dateformat)] %>% as_datetime(), by = "hour") %>% strftime(tz = "UTC")

  vect_open_return_na <- function(timestamp) {

    filedate <- paste0(
      substr(timestamp, 0, 4),
      substr(timestamp, 6, 7),
      substr(timestamp, 9, 10),
      "T",
      substr(timestamp, 12, 13),
      "Z_", variable, ".nc"
    )
    print(filedate)

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

  if(is.null(n_cores)){
    n_cores = parallel::detectCores() - 2
  }
  logfilepath = paste0(dirname(folderpath),"/", variable, "_parallel_log.txt")
  cl <-parallel::makeCluster(n_cores, outfile ="parlog.log")
  doParallel::registerDoParallel(cl)
  result <- foreach(hour = full_date_range) %dopar% {vect_open_return_na(timestamp = hour)}
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



  # define dimensions
  xdim <- ncdim_def("x",units="m",
                    longname="eastward distance from southwest corner of domain in projection coordinates",as.double(x))
  ydim <- ncdim_def("y",units="m",
                    longname="northward distance from southwest corner of domain in projection coordinates",as.double(y))


  ### Time dimensions:
  # the upper one is from the handy guide im using, the lower one from the source
  # ncdf4 files for senorgeCWATM. not sure which to use..
  tunits <- "hours since 1901-01-01 01:00:00"
  source_date = as_datetime("1901-01-01 01:00:00") %>% strftime(tz = "UTC")
  first_date = full_date_range[1]
  basehour <- difftime(first_date, source_date, units =  "hour") %>% as.numeric()
  #basehour <- basehour - 1
  post_hours <- c(1:length(full_date_range))
  hours_since_1901_01 <-basehour+post_hours
  timedim <- ncdim_def(name = "time" ,units = tunits,vals = hours_since_1901_01, unlim = F, calendar = "proleptic_gregorian")

  dlname <- "Longitude of cell center"
  lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
  dlname <- "Latitude of cell center"
  lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
  dlname <- "Lambert_Conform_Conical"
  proj_def <- ncvar_def("lambert_conformal_conic","1",NULL,NULL,longname=dlname,prec="char")

  dunits <- ncatt_get(templatenc,variable,"units")
  current_var_def <- ncvar_def(variable,dunits$value,list(xdim,ydim,timedim),fillvalue,variable,prec="double")
  nc_close(templatenc)

  # creating the new file
  ncout <- nc_create(full_write_fp,list(current_var_def,lon_def,lat_def,proj_def),force_v4=TRUE)
  # put variables
  ncvar_put(ncout,current_var_def,datacube)
  ncvar_put(ncout,lon_def,lon)
  ncvar_put(ncout,lat_def,lat)

  # put additional attributes into dimension and data variables
  ncatt_put(ncout,"x","axis","X")
  ncatt_put(ncout,"x","standard_name","projection_x_coordinate")
  ncatt_put(ncout,"x","_CoordinateAxisType","GeoX")
  ncatt_put(ncout,"y","axis","Y")
  ncatt_put(ncout,"y","standard_name","projection_y_coordinate")
  ncatt_put(ncout,"y","_CoordinateAxisType","GeoY")
  ncatt_put(ncout, variable,"grid_mapping", "lambert_conformal_conic")
  ncatt_put(ncout, variable,"coordinates", "lat lon")

  # put the CRS attributes
  projname <- "lambert_conformal_conic"
  longitude_of_central_meridian = 15
  latitude_of_projection_origin <- 63
  earth_radius <- 6371000
  standard_parallel <- 63
  # false_easting <- 5632642.22547
  # false_northing <- 4612545.65137
  false_easting <- 0
  false_northing <- 0
  ncatt_put(ncout,"lambert_conformal_conic","name",projname)
  ncatt_put(ncout,"lambert_conformal_conic","long_name",projname)
  ncatt_put(ncout,"lambert_conformal_conic","grid_mapping_name",projname)
  ncatt_put(ncout,"lambert_conformal_conic","longitude_of_central_meridian", as.double(longitude_of_central_meridian))
  ncatt_put(ncout,"lambert_conformal_conic","latitude_of_projection_origin", as.double(latitude_of_projection_origin))
  ncatt_put(ncout,"lambert_conformal_conic","standard_parallel", c(standard_parallel, standard_parallel))
  ncatt_put(ncout,"lambert_conformal_conic","false_easting",false_easting)
  ncatt_put(ncout,"lambert_conformal_conic","false_northing",false_northing)
  ncatt_put(ncout,"lambert_conformal_conic","_CoordinateTransformType","Projection")
  ncatt_put(ncout,"lambert_conformal_conic","_CoordinateAxisTypes","GeoX GeoY")

  # add global attributes (TODO: cleanup)
  ncatt_put(ncout,0,"title",paste0("MET Nordic dataset variable ",variable, "(hourly)"))
  ins_text <- paste0("Sourced from MetNordic ++ Downloaded and processed miljotools version ",utils::packageVersion("miljotools"), " (https://github.com/moritzshore/miljotools)")
  ncatt_put(ncout,0,"institution",ins_text)
  history <- paste("Creaed ", date())
  ncatt_put(ncout,0,"history",history)
  ncatt_put(ncout,0,"Package URL", "https://github.com/moritzshore/miljotools")
  ncatt_put(ncout,0,"Conventions","CF=1.6")
  ncatt_put(ncout,0,"source files",folderpath)
  # close the file, writing data to disk
  nc_close(ncout)

  return(full_write_fp)
}

