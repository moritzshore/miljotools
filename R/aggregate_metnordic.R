#' Convert hourly MetNordic files to daily
#'
#' This function takes 24 hourly files from the same day and converts them to
#' an daily format
#'
#' @param directory String: Path to the source files
#' @param day String: day to convert (ie. "20150901")
#' @param outpath String: path to write file
#' @param preview Logical: plot map?
#'
#' @return String: path to written file
#' @export
#'
#'
#' @examples
#' # TODO
#'
#' @import ncdf4
#' @importFrom dplyr  %>%
#' @importFrom abind abind
#' @importFrom stringr str_split str_remove
aggregate_day_metnodric <- function(directory, day, outpath, overwrite = TRUE, preview = TRUE) {
  # START
  short_fp <- list.files(directory, pattern = day)
  long_fp <- list.files(directory, pattern = day, full.names = T)
  dir.create(outpath)


  if(length(short_fp)!= 24){
    warning("only", length(short), "hours detected on", day, "skipping...")
    return(FALSE)
  }

  # LOAD ALL DAYS
  vec_ncvar_get <- function(filename){
    opennc <- nc_open(filename)
    # this will break if proper file structure is not maintained!
    var_name <-  (opennc$var %>% names())[1]
    slice = ncvar_get(opennc, var_name)
    nc_close(opennc)
    return(slice)
  }
  datacube <- lapply(long_fp, vec_ncvar_get) %>% abind(along = 3)
  # dim(datacube) should be x by y by 24


  templatenc <- nc_open(long_fp[1])
  templatenc$dim$time$units = "day"
  templatenc$dim$time$calendar = "stored in filename"

  # this will break if proper file structure is not maintained!
  var_name <-  (templatenc$var %>% names())[1]
  write_fp <- paste0("met_analysis_1_0km_nordic_", day, "-", var_name, ".nc")
  full_write_fp <- paste0(outpath, "/", write_fp)
  if(file.exists(full_write_fp)){
    if(overwrite){
      warning("file already exists, overwriting: ", write_fp)
      removed = file.remove(full_write_fp)
      if(removed == FALSE){stop("error with removing file",full_write_fp )}
    }else{
      warning(write_fp, "already exists, skipping..")
      return(FALSE)
    }
  }

  # precipitation should be summed
  if (var_name == "precipitation_amount") {
    flat_cube <- rowSums(datacube, dims = 2)
  }
  # plot
  if (preview) {
    print(flat_cube %>% image(main = paste0(var_name, " ", day)))
  }

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
  timedim <- ncdim_def(name = "time", units = "day",
                       vals = 1, unlim = F, calendar = "daily")

  dlname <- "Longitude of cell center"
  lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
  dlname <- "Latitude of cell center"
  lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
  dlname <- "Lambert_Conform_Conical"
  proj_def <- ncvar_def("lambert_conformal_conic","1",NULL,NULL,longname=dlname,prec="char")


  dunits <- ncatt_get(templatenc,var_name,"units")
  current_var_def <- ncvar_def(var_name,dunits$value,list(xdim,ydim,timedim),fillvalue,var_name,prec="double")

  nc_close(templatenc)

  ncout <- nc_create(full_write_fp,list(current_var_def,lon_def,lat_def,proj_def),force_v4=TRUE)


  # put variables
  ncvar_put(ncout,current_var_def,flat_cube)
  ncvar_put(ncout,lon_def,lon)
  ncvar_put(ncout,lat_def,lat)

  # put additional attributes into dimension and data variables
  ncatt_put(ncout,"x","axis","X")
  ncatt_put(ncout,"x","standard_name","projection_x_coordinate")
  ncatt_put(ncout,"x","_CoordinateAxisType","GeoX")
  ncatt_put(ncout,"y","axis","Y")
  ncatt_put(ncout,"y","standard_name","projection_y_coordinate")
  ncatt_put(ncout,"y","_CoordinateAxisType","GeoY")
  ncatt_put(ncout, var_name,"grid_mapping", "lambert_conformal_conic")
  ncatt_put(ncout, var_name,"coordinates", "lat lon")

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

  # add global attributes
  ncatt_put(ncout,0,"title",paste0("MET Nordic dataset variable ",var_name, "(daily)"))
  ins_text <- paste0("Sourced from MetNordic, Downloaded and processed NIBIO using miljotools version ",packageVersion("miljotools"), " (https://github.com/moritzshore/miljotools)")
  ncatt_put(ncout,0,"institution",ins_text)
  history <- paste("Creaed ", date())
  ncatt_put(ncout,0,"history",history)
  ncatt_put(ncout,0,"Package URL", "https://github.com/moritzshore/miljotools")

  ncatt_put(ncout,0,"Conventions","CF=1.6")
  ncatt_put(ncout,0,"source files",directory)


  # Get a summary of the created file:
  # if(preview){print(ncout)}

  # close the file, writing data to disk
  nc_close(ncout)

  return(full_write_fp)
}




