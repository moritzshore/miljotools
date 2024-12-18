#' Convert MetNordic Files to CWatM Meteo Input
#'
#' This function converts a projected metnodric file to be compatible with
#' CWatm. This function mainly just changes the dimension names from easting and
#' northing to x and y but has a crap load of boiler plate because... well..
#' ncdf4.
#'
#' @param infile path to projected MetNordic .nc file as created by `metnordic_reproject()`
#' @param outfile path and file name to desired output file
#'
#' @returns path to outfile
#' @export
#'
#' @examples
#' #TODO
#' @import ncdf4
cwatm_convert_nc <- function(infile, outfile){
  ### Re-reading it and changing attributes to x and y (This is only needed for CWATM.. TODO: seperate function?)
  to_reform <- nc_open(infile)
  main_var <- to_reform$var %>% names() %>% nth(1)

  ## getting data

  met_var <- ncvar_get(to_reform, main_var)
  met_var_att <- ncatt_get(to_reform, main_var)
  easting <- ncvar_get(to_reform,"easting")
  easting_att <- ncatt_get(to_reform, "easting")
  northing <- ncvar_get(to_reform,"northing")
  northing_att <- ncatt_get(to_reform, "northing")
  time <- ncvar_get(to_reform, "time")
  time_att <- ncatt_get(to_reform, "time")
  crs <- ncvar_get(to_reform, "crs")
  crs_att <- ncatt_get(to_reform, "crs")

  ## Dim Definitions
  xdef <- ncdf4::ncdim_def(name = "x", units = "meters", vals =  easting, longname = "x", )
  ydef <- ncdf4::ncdim_def(name = "y", units = "meters", vals =  northing, longname = "y")
  timedef <- ncdf4::ncdim_def(name = "time", units = time_att$units, vals = time, calendar = time_att$calendar, longname = time_att$long_name)

  ## Var Definitions
  fillvalue <- 1e32
  main_var_def <- ncvar_def(name = main_var, units = met_var_att$units, dim = list(xdef,ydef,timedef), missval = met_var_att$`_FillValue`, prec = "float")
  crs_def <- ncvar_def(name = "crs", units = "", dim = list(), prec = "integer")

  # Creating the new ncfile
  proj_nc <- nc_create(filename = outfile, vars = list(crs_def, main_var_def), force_v4 = T) # the order of this might be important

  # Adding data
  ncvar_put(nc = proj_nc, varid = main_var, vals = met_var)

  # Adding Attributes
  ncatt_put(proj_nc, varid = main_var, attname = "grid_mapping", "crs")

  ncatt_put(proj_nc, varid = "crs", attname = "crs_wkt", crs_att$crs_wkt)
  ncatt_put(proj_nc, varid = "crs", attname = "spatial_ref", crs_att$spatial_ref)
  ncatt_put(proj_nc, varid = "crs", attname = "proj4", crs_att$proj4)
  ncatt_put(proj_nc, varid = "crs", attname = "geotransform", crs_att$geotransform)

  ncatt_put(nc = proj_nc, varid = 0 ,attname = "Conventions", attval = ncatt_get(to_reform, 0, "Conventions")[[2]])
  ncatt_put(nc = proj_nc, varid = 0 ,attname = "created_by", attval = ncatt_get(to_reform, 0, "created_by")[[2]])
  ncatt_put(nc = proj_nc, varid = 0 ,attname = "date", attval = ncatt_get(to_reform, 0, "date")[[2]])
  ncatt_put(nc = proj_nc, varid = 0 ,attname = "institution", attval = ncatt_get(to_reform, 0, "institution")[[2]])
  ncatt_put(nc = proj_nc, varid = 0 ,attname = "history", attval = ncatt_get(to_reform, 0, "history")[[2]])

  # Saving
  nc_close(to_reform)
  nc_close(proj_nc)

  return(outfile)
}
