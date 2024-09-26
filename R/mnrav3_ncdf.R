### MNRV3 NCDF Support
# Date: Sep. 26, 2024
# Author: Moritz Shore
# Purpose: Save the server request NCDF files as NCDF instead of converting immediately to dataframes. This is quite overcomplicated because there doesnt seem to be any functional way of opening the server requested files and then just writing them to disk.

#' Read and write ncdf files
#'
#' instead of coverting them to dataframe format
#'
#' @param url
#' @param savefiles
#'
#' @return
#' @keywords internal
#'
#' @examples
read_write_ncdf <- function(url, savefiles){

  # Download first file to get the dimensios set, then loop through following
  # Files

  # open first Netcdf file
  ncin_crop <- nc_open_retry(url[2])

  # getting numbers
  # do once
  lat <- ncvar_get(ncin_crop, "latitude")
  lon <- ncvar_get(ncin_crop, "longitude")
  at2<-ncvar_get(ncin_crop, "air_temperature_2m")

  nc_close(ncin_crop)
  # dim(lat)[2]
  # dim(lat)[1]
  # defning dims
  mytime <- ncdim_def(name = "time", units = "hour", vals =1, unlim = F)
  mylat <- ncdim_def(name = "y", units = "m",longname = "projection_y_coordinate", vals = 1:dim(lat)[2])
  mylon <- ncdim_def(name = "x", units = "m",longname = "projection_y_coordinate", vals = 1:dim(lat)[1])

  # define
  lat_vals <- ncvar_def("lat_vals", units = "deg", dim = list(mylat,mylon, mytime))
  lon_vals <- ncvar_def("lon_vals", units = "deg", dim = list(mylat,mylon, mytime))
  at2_defined <- ncvar_def("temp", units = "K", dim = list(mylat,mylon, mytime))

  # write
  filename = savefiles[3]
  helloworld <- nc_create(filename, vars = list(lat_vals, lon_vals, at2_defined))

  ncvar_put(helloworld, "temp", vals = at2)
  ncvar_put(helloworld, "lat_vals", vals = lat)
  ncvar_put(helloworld, "lon_vals", vals = lon)
  ncvar_put(helloworld, "time", vals = 1)

  # close
  nc_close(helloworld)


  testing <- nc_open(filename)

  ncvar_get(testing, "temp")
  ncvar_get(testing, "lat_vals")
  ncvar_get(testing, "lon_vals")


  # repeat for all following files
  for (idate in c(2:length(url))) {
    # print status
    cat("\r","downloading files ", " (", idate, "/", length(url), ")", sep = "")



  }
}
