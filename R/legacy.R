#' Generate SWAT+ weather input for any watershed in the Nordics.(LEGACY)
#'
#' **LEGACY**. Note: this function is no longer supported
#'
#' This function combines 3 `miljotools` functions in a single gridded data
#' retrieval and processing pipeline to write and assign weather data to a SWAT+
#' setup. The functions involved are
#'
#' 1. `get_metno_reanalysis3()` downloads and processes the hourly gridded
#' reanalysis data for the nordics
#'
#' 2. `reanalysis3_daily()` converts these hourly timeseries into daily.
#'
#' 3. `reanalysis3_swatinput()` converts these timeseries into a SWAT+
#' compatible format as well as generating the weather generator, and updating
#' SWAT+ input files with the help of the R-package `SWARTprepR`
#'
#' For more details please see the help pages of the individual functions. Also
#' please note the the package `SWATprepR` is required for this pipeline.
#'
#' @param area The catchment area to retrieve data for. (must be a shapefile)
#' @param swat_setup The path to your SWAT+ setup (input files, aka TxtInOut)
#' @param grid_resolution (integer) desired resolution of downloaded grid in kilometers.
#' @param directory directory to download and process data in
#' @param from start of the to-be-dowloaded timeseries (ie. and min: "2012-09-01
#'   10:00:00")
#' @param to end of the to-be-dowloaded timeseries (ie. and max: "2023-01-31
#'   10:00:00")
#' @param area_buffer optional buffer in meters around the provided area
#' @param verbose print status messages?
#' @param precision Optional, which precision (integer) should the hourly data
#'   be rounded down to when converted from daily to hourly. Default is '2'
#'   decimal places.
#' @param write_wgn would you like to calculate and write the weather generator?
#' @param sqlite_path optionally, you can pass the path of your .sqlite file in
#'   order to update the database with your new met files
#'
#' @author Moritz Shore, Svajunas Plunge
#'
#' @export
#'
swat_weather_input_chain <-
  function(area,
           swat_setup,
           grid_resolution = 1,
           directory = NULL,
           from = NULL,
           to = NULL,
           area_buffer = 1500,
           verbose = TRUE,
           precision = 2,
           write_wgn = TRUE,
           sqlite_path = NULL) {

    path1 <- get_metno_reanalysis3(
      area,
      directory = directory,
      fromdate =  from,
      todate = to,
      area_buffer = area_buffer,
      preview = verbose,
      grid_resolution = grid_resolution
    )

    path2 <- reanalysis3_daily(
      path = path1,
      outpath = directory,
      verbose = verbose,
      precision = precision
    )


    path3 <- reanalysis3_swatinput(
      path = path2,
      swat_setup = swat_setup,
      write_wgn = write_wgn,
      sqlite_path = sqlite_path,
      verbose = verbose
    )

    print("miljotools: pipeline finished!")
  }
