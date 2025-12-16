#' Merge MET Nordic Files (daily)
#'
#' This function merges **daily** MET Nordic files into a single .nc file for a
#' single variable. The input data should be the output data of
#' `metnordic_aggregate()`. This function provides input for
#' `metnordic_reproject()`. For **hourly** merging, please use `metnordic_merge_hourly()`
#'
#' Code largely adapted from this handy guide:
#' ([link](https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-projected-netcdf-file))
#' from Pat Bartlein, bartlein@uoregon.edu
#'
#' @seealso [metnordic_aggregate()] [metnordic_reproject()] [metnordic_merge_hourly()]
#' @author Moritz Shore
#'
#' @param folderpath String: Outpath of files from `metnordic_aggregate()`.
#' @param variable String: [MET Nordic variable](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters) to be used (**NOTE:** don't forget suffix such as 'mean', 'sum', 'min', or 'max').
#' @param outpath String: Folder path where to write files.
#' @param overwrite Logical: Overwrite existing file?
#' @param verify Logical: check if all dates in date range exist?
#'
#' @returns file path of generated files
#' @export
#'
#' @examples
#' # TODO
#' @importFrom abind abind
#' @importFrom dplyr %>% first last
#' @importFrom stringr str_split
metnordic_merge_daily <- function(folderpath, variable, outpath, overwrite = FALSE, verify = FALSE) {



  # testing par set:
  #
  # targetnc  <- nc_open("../staging_ground/test_miljotools/testing_pr_nor2.nc")
  # targetnc
  #
  # # PARS
  # folderpath <- "../staging_ground/test_miljotools/bigdaily/"
  # variable = "precipitation_amount"
  # outpath = "../staging_ground/test_miljotools/bigyearly/"
  # overwrite = TRUE
  #START

  # here we grab NC files and filter then by variable.
  short_fps <- list.files(folderpath, pattern = "*.nc")
  long_fps <- list.files(folderpath, pattern = "*.nc", full.names = T)
  short_fps_filt <- short_fps[(grepl(x = short_fps, pattern = variable) %>% which())]
  long_fps_filt <- long_fps[(grepl(x = short_fps, pattern = variable) %>% which())]

  if(verify){
    short_fps_filt  %>% str_remove(variable) %>% str_remove(".nc") %>% str_split("_", simplify = T) -> myvec0
    myvec1 <- suppressWarnings(as.numeric(myvec0))
    myvec2 <- myvec1[!is.na(myvec1)]
    dtcode <- myvec2[myvec2 > 19010101]
    year = substr(dtcode, 0,4)
    month = substr(dtcode, 5,6)
    day = substr(dtcode, 7,8)
    hour = substr(dtcode, 10,11)
    paste0(year, "-", month, "-", day) %>% lubridate::as_date() -> dates

    dates %>% range() -> daterange
    seq(daterange[1], daterange[2],by = "day") -> fulldate
    which(dates != fulldate) -> issues
    if(issues %>% length() == 0){
      mt_print(TRUE, "metnordic_merge_daily", "No missing dates detected")
    }else{
    mt_print(TRUE, "metnordic_merge_daily", "Missing dates detected!")
    stop(paste0("missing date before : ",dates[issues %>% min()], "\n",
                "file before: ",short_fps_filt[issues %>% min()] ))
    }
  }


  # This opens all the files at once for the given varible, and then binds them
  # along the time axis (along = 3) to create a data cube.
  vect_open_return <- function(filepaths){
    ncfile <- nc_open(filepaths)
    slice <- ncvar_get(ncfile, variable)
    nc_close(ncfile)
    return(slice)
  }
  datacube <- lapply(long_fps_filt, vect_open_return) %>% abind::abind(along = 3)

  # here we determine the correct format for the time dimension
  # "days since 1901-01-01 00:00:00.0"
  # note, UNSTABLE! dependent on file path ([,6] and [,1])
  daily_filenames <- ((str_split(short_fps_filt, pattern = "_", simplify = T)[,6]) %>% str_split(pattern = "-", simplify = T))[,1]
  dateformat <- paste0(substring(daily_filenames, 1,4), "-", substring(daily_filenames, 5,6), "-", substring(daily_filenames, 7,8)) %>% as.Date()
  dayssince1901 <- (dateformat-as.Date("1901-01-01")) %>% as.numeric()
  daterange <- paste0(daily_filenames %>% dplyr::first(),"to", daily_filenames %>% dplyr::last())

  ### the rest is largely copied from the other metnordic functions
  # with a few adjustments for the time dimension.

  # create the file path to be written, check if it already exists. if the
  # overwrite flag is enabled, then the existing file is delted to be created
  # again. if not, the function skips this file.
  write_fp <- paste0("MetNordic_", variable, "_", daterange, ".nc")
  full_write_fp <- paste0(outpath, "/", write_fp)
  dir.create(outpath, showWarnings = F)
  if(file.exists(full_write_fp)){
    if(overwrite){
      warning(" file already exists, overwriting: ", write_fp)
      removed = file.remove(full_write_fp)
      if(removed == FALSE){stop("error with removing file",full_write_fp )}
    }else{
      warning(write_fp, "already exists, skipping..")
      return(FALSE)
    }
  }

  # opening the first nc file to serve as a template. many attributes are
  # extracted
  templatenc <- nc_open(long_fps_filt[1])
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

  # define dimensions
  xdim <- ncdim_def("x",units="m",
                    longname="eastward distance from southwest corner of domain in projection coordinates",as.double(x))
  ydim <- ncdim_def("y",units="m",
                    longname="northward distance from southwest corner of domain in projection coordinates",as.double(y))
  ### Time dimensions:
  # the upper one is from the handy guide im using, the lower one from the source
  # ncdf4 files for senorgeCWATM. not sure which to use..
  tunits <- "days since 1900-01-01 00:00:00.0 -0:00"
  tunits <- "days since 1901-01-01 00:00:00.0"
  timedim <- ncdim_def(name = "time",units = tunits,vals = as.double(dayssince1901), unlim = F, calendar = "proleptic_gregorian")

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
  ncatt_put(ncout,0,"title",paste0("MET Nordic dataset variable ",variable, "(daily)"))
  ins_text <- paste0("Sourced from MetNordic, Downloaded and processed NIBIO using miljotools version ",utils::packageVersion("miljotools"), " (https://github.com/moritzshore/miljotools)")
  ncatt_put(ncout,0,"institution",ins_text)
  history <- paste("Creaed ", date())
  ncatt_put(ncout,0,"history",history)
  ncatt_put(ncout,0,"Package URL", "https://github.com/moritzshore/miljotools")
  ncatt_put(ncout,0,"Conventions","CF=1.6")
  ncatt_put(ncout,0,"source files",folderpath)
  # close the file, writing data to disk
  nc_close(ncout)

  # return file path:
  return(full_write_fp)
}



