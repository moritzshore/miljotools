#' Reproject MetNordic Data
#'
#' Takes in MetNordic .nc files as created by `metnordic_download()` and
#' reprojects them to desired projection.
#'
#' @param filepath path to .nc file to be reprojected
#' @param outfile filepath of .nc file to be created. (if the same file path is used, .nc file will be overwritten)
#' @param projstring desired projection in [proj4 format](https://epsg.io/docs). By default, [UTM33N](https://epsg.io/32633) will be used.
#'
#' @returns filepath to reprojected file
#' @export
#'
#' @examples
#' # TODO
#' @importFrom terra rast crs project writeCDF
#' @importFrom dplyr  %>%
#' @importFrom ncdf4 nc_open ncatt_get
metnordic_reproject <- function(filepath, outfile, projstring = NULL){
  # how to reproject the metnorodic to UTM 33 N
  # filepath <- "../staging_ground/test_miljotools/mndl_day2%/met_analysis_1_0km_nordic_20210101-air_temperature_2m.nc"
  ncfile <- nc_open(filepath)
  varid <- (ncfile$var %>% names())[1]
  varname <- ncdf4::ncatt_get(ncfile, varid)
  ncfile_spatrast <- terra::rast(filepath)
  metnordic_crs <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06"
  if(is.null(projstring)){
    projstring <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +type=crs"
  }
  terra::crs(ncfile_spatrast) <- metnordic_crs
  nc_reprojected <- terra::project(ncfile_spatrast, projstring)

  # todo, add more stuff here
  instituion = "institution=Sourced from MetNordic, processed by NIBIO"
  history = paste0("history=created by miljotools")

  test = terra::writeCDF(
    x = nc_reprojected,
    filename = outfile,
    varname = varid,
    unit = varname$units,
    atts = c(instituion, history),
    overwrite = TRUE
  )

  return(test)
}
