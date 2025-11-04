#' Convert MET Nordic Files to CWatM Meteo Input
#'
#' This function converts a projected MET Nordic file (from
#' `metnordic_reproject()`) to be compatible with CWatm. This function mainly
#' just changes the dimension names from easting and northing to x and y and
#' swaps variable order but has a crap load of boiler plate because... well..
#' ncdf4. Supported variables are: tmax, tmin, prec, RH, shortwave downwelling, air pressure, wind speed
#'
#' Code largely adapted from this handy guide:
#' ([link](https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-projected-netcdf-file))
#' from Pat Bartlein, bartlein@uoregon.edu
#'
#' @seealso [metnordic_reproject()]
#'
#' @author Moritz Shore
#'
#' @param infile String: path to projected MET Nordic .nc file as created by `metnordic_reproject()`
#' @param outfile String: path and file name to desired output file
#' @param altitude Only needed for converting to surface pressure. Path to file which contains altitude for the grid (.nc, ie as downloaded by `metnordic_download()`)
#' @param temperature Only needed for converting to surface pressure. Path to file which contains temperature data for a data cube of the same dimension.
#' @param verbose Logical: print to console?
#'
#' @returns Path to outfile
#' @export
#'
#' @examples
#' #TODO
#' @import ncdf4
cwatm_convert_nc <- function(infile, outfile, altitude = NULL, temperature = NULL, verbose = FALSE){
  ### Re-reading it and changing attributes to x and y (This is only needed for CWATM.. TODO: seperate function?)
  to_reform <- ncdf4::nc_open(infile)
  main_var <- to_reform$var %>% names() %>% dplyr::nth(1)

  ## getting data

  met_var <- ncdf4::ncvar_get(to_reform, main_var)
  met_var_att <- ncdf4::ncatt_get(to_reform, main_var)
  easting <- ncdf4::ncvar_get(to_reform,"easting")
  easting_att <- ncdf4::ncatt_get(to_reform, "easting")
  northing <- ncdf4::ncvar_get(to_reform,"northing")
  northing_att <- ncdf4::ncatt_get(to_reform, "northing")
  time <- ncdf4::ncvar_get(to_reform, "time")
  time_att <- ncdf4::ncatt_get(to_reform, "time")
  crs <- ncdf4::ncvar_get(to_reform, "crs")
  crs_att <- ncdf4::ncatt_get(to_reform, "crs")

  ## Dim Definitions
  xdef <- ncdf4::ncdim_def(name = "x", units = "meters", vals =  easting, longname = "x", )
  ydef <- ncdf4::ncdim_def(name = "y", units = "meters", vals =  northing, longname = "y")
  # the calendar attribute gets lost in the terra reproject function from metnordic_reproject().
  timedef <- ncdf4::ncdim_def(name = "time", units = time_att$units, vals = time, calendar = "proleptic_gregorian", longname = time_att$long_name)

  ## Var Definitions
  fillvalue <- 1e32
  if(main_var == "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time_sum"){
    mt_print(verbose, "cwatm_convert_nc", "Altering shortwave radiation units..")
    main_var_def <- ncdf4::ncvar_def(name = main_var, units =  "W m-2", dim = list(xdef,ydef,timedef), missval = met_var_att$`_FillValue`, prec = "float")
  }else if(main_var =="air_pressure_at_sea_level_mean" ){
    mt_print(verbose, "cwatm_convert_nc", "renaming to surface presure...")
    main_var_def <- ncdf4::ncvar_def(name ="air_pressure_at_surface_mean" , units = met_var_att$units, dim = list(xdef,ydef,timedef), missval = met_var_att$`_FillValue`, prec = "float")
  }else{
    main_var_def <- ncdf4::ncvar_def(name = main_var, units = met_var_att$units, dim = list(xdef,ydef,timedef), missval = met_var_att$`_FillValue`, prec = "float")
  }

  crs_def <- ncdf4::ncvar_def(name = "crs", units = "", dim = list(), prec = "integer")

  # Creating the new ncfile
  proj_nc <- ncdf4::nc_create(filename = outfile, vars = list(crs_def, main_var_def), force_v4 = T) # the order of this might be important

  # Adding data (some variables need conversion)
  # if the variable is humidity, then we need to multiply by 100 for CWATM standards..
  if(main_var == "relative_humidity_2m_mean"){
   met_var2 = met_var * 100
   mt_print(verbose, "cwatm_convert_nc", "Converting RH..")
   ncdf4::ncvar_put(nc = proj_nc, varid = main_var, vals = met_var2)
  }else if(main_var == "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time_sum"){
    # if the variable is showtwave flux, then we need to remove the "Seconds" unit.
    mt_print(verbose, "cwatm_convert_nc", "Converting shortwave radiation..")
    met_var2 = met_var / 86400
    ncdf4::ncvar_put(nc = proj_nc, varid = main_var, vals = met_var2)
  }else if(main_var == "air_pressure_at_sea_level_mean"){
    # if the variable is air pressure, we need to convert to surface pressure
    air_pressure_surface <- convert_sea_to_surface(
      met_var = met_var,
      altitude = altitude,
      temperature = temperature,
      verbose = verbose
    )
    # add the converted values
    mt_print(verbose, "cwatm_convert_nc", "Applying converted surface pressure values..")
    ncdf4::ncvar_put(nc = proj_nc, varid = "air_pressure_at_surface_mean", vals = air_pressure_surface)
    main_var = "air_pressure_at_surface_mean"
  } else{
    ncdf4::ncvar_put(nc = proj_nc, varid = main_var, vals = met_var)
  }

  # Adding Attributes
  ncdf4::ncatt_put(proj_nc, varid = main_var, attname = "grid_mapping", "crs")
  ncdf4::ncatt_put(proj_nc, varid = "crs", attname = "crs_wkt", crs_att$crs_wkt)
  ncdf4::ncatt_put(proj_nc, varid = "crs", attname = "spatial_ref", crs_att$spatial_ref)
  ncdf4:: ncatt_put(proj_nc, varid = "crs", attname = "proj4", crs_att$proj4)
  ncdf4::ncatt_put(proj_nc, varid = "crs", attname = "geotransform", crs_att$geotransform)

  ncdf4::ncatt_put(nc = proj_nc, varid = 0 ,attname = "Conventions", attval = ncdf4::ncatt_get(to_reform, 0, "Conventions")[[2]])
  ncdf4::ncatt_put(nc = proj_nc, varid = 0 ,attname = "created_by", attval = ncdf4::ncatt_get(to_reform, 0, "created_by")[[2]])
  ncdf4::ncatt_put(nc = proj_nc, varid = 0 ,attname = "date", attval = ncdf4::ncatt_get(to_reform, 0, "date")[[2]])
  ncdf4::ncatt_put(nc = proj_nc, varid = 0 ,attname = "institution", attval = ncdf4::ncatt_get(to_reform, 0, "institution")[[2]])
  ncdf4::ncatt_put(nc = proj_nc, varid = 0 ,attname = "history", attval = ncdf4::ncatt_get(to_reform, 0, "history")[[2]])

  # Saving
  ncdf4::nc_close(to_reform)
  ncdf4::nc_close(proj_nc)

  return(outfile)
}

convert_sea_to_surface <- function(met_var, altitude, temperature, verbose){
  # GRAB ALTITUDE DATA
  #altitude = "../met-nordic-paper/CWATM/MET/METNORDIC/download/met_analysis_1_0km_nordic_20130101T00Z_air_pressure_at_sea_level.nc"
  altitude %>% ncdf4::nc_open() -> alt_nc
  alt_nc %>% ncdf4::ncvar_get("altitude") -> alt_grid

  if(((met_var[,,1] %>% dim() == alt_grid %>% dim()) %>% all()) == FALSE){
    stop("Spatial dimensions of altitude grid do not match dimensions of pressure data!")
  }
  ## TEMP TEMPERATURE
  # temperature dummy data
  someData <- rep(273.15, 732)
  temp_temp <- array(someData, c(203, 204, 732))
  warning("still using dummy temp")
  warning("remember to convert to K")

  if((((temp_temp %>% dim()) == (met_var %>% dim())) %>% all()) == FALSE){
    stop("Temperature data dimensions do not match that of pressure data!")
  }
  xs =c(1:dim(met_var)[1])
  ys = c(1:dim(met_var)[2])
  days = c(1:dim(met_var)[3])

  # MATRIX TO BE FILLED WITH CONVERTED DATA
  placeholder <- rep(NA, dim(met_var)[3])
  air_pressure_surface <- array(placeholder, dim(met_var))

  dim(met_var)[1] * dim(met_var)[2] * dim(met_var)[3] -> tot_ops
  i = 0
  for (day in days) {
    for (y in ys) {
      for (x in xs) {
        i = i + 1
        # mt_print(verbose, "cwatm_convert_nc",
        #          paste0("Converting cells..(", 100*((i/tot_ops) %>% round(4)), "%)"),
        #          paste0("[", x, ",", y, ",", day, "]"),
        #          rflag = T)
        sea_level_pressure = met_var[x,y,day]
        altitude = alt_grid[x,y]
        temperature = temp_temp[x,y,day] # temperature at ~sea~ level... [K]
        P_b = sea_level_pressure # pressure at sea level (from met nordic) [Pa]
        h = altitude # height (m, from MN)
        R = 8.31432 # universal gas constant (N*m / mol * K)
        g_0 = 9.80665 # grav. constant (m/2^2)
        M = 0.0289644 # molar mass of Earth’s air [kg/mol]
        h0 = 0 # reference altitude (0 for sea level?)
        P = P_b * exp(-g_0 * M * (h - h0) / (R * temperature))
        air_pressure_surface[x,y,day] = P
      }
    }
    mt_print(verbose, "cwatm_convert_nc",
             paste0("Converting cells..(", 100*((i/tot_ops) %>% round(2)), "%)"),
             paste0("[", day, "/", dim(met_var)[3], " days]"),
             rflag = T)
  }
  if(verbose){cat("\n")}
  return(air_pressure_surface)
}
