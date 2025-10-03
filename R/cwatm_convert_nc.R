#' Convert MET Nordic Files to CWatM Meteo Input
#'
#' This function converts a projected MET Nordic file (from
#' `metnordic_reproject()`) to be compatible with CWatm. This function mainly
#' just changes the dimension names from easting and northing to x and y and
#' swapes variable order but has a crap load of boiler plate because... well..
#' ncdf4.
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
#'
#' @returns Path to outfile
#' @export
#'
#' @examples
#' #TODO
#' @import ncdf4
cwatm_convert_nc <- function(infile, outfile){
  ### Re-reading it and changing attributes to x and y (This is only needed for CWATM.. TODO: seperate function?)
  to_reform <- ncdf4::nc_open(infile)
  main_var <- to_reform$var %>% names() %>% nth(1)

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
  main_var_def <- ncdf4::ncvar_def(name = main_var, units = met_var_att$units, dim = list(xdef,ydef,timedef), missval = met_var_att$`_FillValue`, prec = "float")
  crs_def <- ncdf4::ncvar_def(name = "crs", units = "", dim = list(), prec = "integer")

  # Creating the new ncfile
  proj_nc <- ncdf4::nc_create(filename = outfile, vars = list(crs_def, main_var_def), force_v4 = T) # the order of this might be important

  # Adding data
  ncdf4::ncvar_put(nc = proj_nc, varid = main_var, vals = met_var)

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
