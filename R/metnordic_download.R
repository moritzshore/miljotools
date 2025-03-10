#' Download a MET Nordic Data URL
#'
#' Downloads MET Nordic re-analysis data using an OPENDAP url. This function
#' ideally takes input from `metnordic_buildquery()` and provides input for
#' `metnordic_aggregate()`.
#'
#' Code largely adapted from this handy guide:
#' ([link](https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-projected-netcdf-file))
#' from Pat Bartlein, bartlein@uoregon.edu
#'
#' @seealso [metnordic_buildquery()] [metnordic_aggregate()]
#'
#' @author Moritz Shore
#'
#' @param url String: OPENDAP url to be downloaded (from `metnordic_buildquery()).
#' @param outdir String: Location where the .nc file should be written.
#' @param vars Vector: MET Nordic Variables to extract. See: [MET Nordic variables](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters)
#' @param overwrite Logical: Overwrite files if they already exist?
#' @param verbose Logical: Print preview?
#'
#' @return String: file paths to generated files
#' @export
#'
#' @examples
#' #TODO
#' @importFrom dplyr  %>%
#' @import ncdf4
#' @importFrom stringr str_remove str_replace str_replace
metnordic_download <- function(url, outdir, vars, overwrite = FALSE, verbose = TRUE){
  # url <- "https://thredds.met.no/thredds/dodsC/metpparchivev3/2012/09/01/met_analysis_1_0km_nordic_20120901T10Z.nc?x[448:1:652],y[868:1:1071],latitude[868:1:1071][448:1:652],longitude[868:1:1071][448:1:652],altitude[868:1:1071][448:1:652],air_temperature_2m[0:1:0][868:1:1071][448:1:652],integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time[0:1:0][868:1:1071][448:1:652],relative_humidity_2m[0:1:0][868:1:1071][448:1:652],precipitation_amount[0:1:0][868:1:1071][448:1:652],wind_speed_10m[0:1:0][868:1:1071][448:1:652],wind_direction_10m[0:1:0][868:1:1071][448:1:652]"
  # TODO: add nc_open_retry

  # create filename and check if it already does. if it does, do not download.
  filename <- ((url %>% stringr::str_split(pattern = ".nc", simplify = T))[1,1] %>% stringr::str_split("/", simplify = T)) %>% as.list() %>% last()
  ncfname <- paste0(outdir,"/", filename, ".nc")
  if(file.exists(ncfname)){
    if(overwrite == FALSE){
      warning(filename, " already exists, not downloading")
      return(ncfname)
    }else{
      warning(filename, " already exists, overwriting")
      file.remove(ncfname)
    }

  }

  # Open file and grab latitude, longitude, x and  y
  ncin <- nc_open_retry(url)

  # if the file download failed, ncin should be null and we can exit the
  # function early.
  if(is.null(ncin)){return(NULL)}

  lon <- ncvar_get(ncin,"longitude")
  lat <- ncvar_get(ncin,"latitude")
  nlat <- dim(lat)
  nlon <- dim(lon)
  x <- ncvar_get(ncin,"x")
  xlname <- ncatt_get(ncin,"x","standard_name")
  xunits <- ncatt_get(ncin,"x","units")
  nx <- dim(x)
  y <- ncvar_get(ncin,"y")
  ylname <- ncatt_get(ncin,"y","standard_name")
  yunits <- ncatt_get(ncin,"y","units")
  ny <- dim(y)

  # get time [broken] this does not work because of the strange things metnordic
  # does with the time dimension:
  # time <- ncvar_get(ncin,"time",start = )


  # define dimensions
  xdim <- ncdim_def("x",units="m",
                    longname="eastward distance from southwest corner of domain in projection coordinates",as.double(x))
  ydim <- ncdim_def("y",units="m",
                    longname="northward distance from southwest corner of domain in projection coordinates",as.double(y))
  timedim <- ncdim_def(name = "time", units = "hour",
                       vals = 1, unlim = F, calendar = "hourly")



  dlname <- "Longitude of cell center"
  lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
  dlname <- "Latitude of cell center"
  lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
  dlname <- "Lambert_Conform_Conical"
  proj_def <- ncvar_def("lambert_conformal_conic","1",NULL,NULL,longname=dlname,prec="char")
  fillvalue <- 1e32

  # creates nc file for given var
  vectorized_create_nc <- function(variable){
    varncfname <- ncfname %>% stringr::str_replace(".nc", paste0("_",variable, ".nc"))
    dlname <- ncatt_get(ncin,variable,"standard_name")
    dunits <- ncatt_get(ncin,variable,"units")
    current_var_def <- ncvar_def(variable,dunits$value,list(xdim,ydim,timedim),fillvalue,dlname$value,prec="double")

    date <- filename %>% stringr::str_remove("met_analysis_1_0km_nordic_")
    ncout <- nc_create(varncfname,list(current_var_def,lon_def,lat_def,proj_def),force_v4=TRUE)

    # Getting variable
    var_array <- ncvar_get(ncin,variable)
    # print a preview of the file
    if(verbose){
      grid <- expand.grid(x=x, y=y)
      cutpts <- seq(min(var_array),max(var_array), length = 10)
      if(all(cutpts == 0)){ cutpts <- c(1:10)}
      print(lattice::levelplot(main = paste(date, variable), var_array ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T,
                               col.regions=c(grDevices::rgb(1,1,1),RColorBrewer::brewer.pal(9,"Blues"))))
    }

    # put variables
    ncvar_put(ncout,current_var_def,var_array)
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

    # add global attributes
    ncatt_put(ncout,0,"title",paste0("MET Nordic dataset variable",variable))
    ncatt_put(ncout,0,"institution","Sourced from MetNordic, (met.no) Downloaded and processed by NIBIO")
    history <- paste("Creaed by miljotools version",utils::packageVersion("miljotools"), "on", date())
    ncatt_put(ncout,0,"history",history)
    ncatt_put(ncout,0,"Package URL", "https://github.com/moritzshore/miljotools")
    ncatt_put(ncout,0,"Conventions","CF=1.6")
    ncatt_put(ncout,0,"source URL",url)

    # close the file, writing data to disk
    nc_close(ncout)

    return(varncfname)
  }

  # creating
  lapply(vars, vectorized_create_nc) %>% return()
}
