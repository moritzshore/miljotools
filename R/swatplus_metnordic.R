#' Create SWAT+ meteo input from MetNo Reanalysis3 data
#'
#' Takes data gathered by `get_metno_reanalysis3()` and downscaled by
#' `reanalysis3_daily()` and creates SWAT+ meteo input files and weather
#' generators for your SWAT+ input with help from SWATprepR.
#'
#' **NOTE** Default parameter value for `mn_variables` in `get_metno_reanalysis3()`  is required for this to work.
#'
#' **NOTE** package `SWATprepR` is required for this function
#'
#' https://github.com/biopsichas/SWATprepR
#'
#' @param path path to daily data provided by `reanalysis3_daily()`
#' @param start optional parameter to define start date of time series
#' @param end optional parameter to define end date of time series
#' @param sqlite_path path to your SWAT+ sqlite file (only needed if you wish to
#'   update your database). Warning: start and end parameters will be ignored in this case (SWATprepR limitation)
#' @param verbose print status?
#' @param write_wgn calculate and write the weather generator? defaults to true. (for now just based on station #1 (bottom left))
#' @param swat_setup path to your SWAT+ setup. (Required!)
#' @param backup (logical, defaults to true) creates a backup of your swat folder before modification
#'
#' @return Files are generated in provided paths
#' @export
#'
#' @author Moritz Shore, Svajunas Plunge
#'
#' @importFrom dplyr last nth
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom stringr str_split str_remove
#' @importFrom writexl write_xlsx
#' @importFrom crayon green italic
reanalysis3_swatinput <-
  function(path,
           swat_setup,
           write_wgn = TRUE,
           start = NA,
           end = NA,
           sqlite_path = NULL,
           verbose = FALSE,
           backup = TRUE) {

    # create a backup of the SWAT+ setup
    if(backup){
      backuppath <- paste0(dirname(swat_setup), "/miljotools_swat_backup")
      dir.create(backuppath, showWarnings = F)
      file.copy(swat_setup, backuppath, recursive = T)
      if(verbose){cat(green(italic("backing up SWAT directory in")), underline(backuppath), "\n")}
    }


    # Check that SWATprepR is installed
    if ("SWATprepR" %in% utils::installed.packages()) {
      # if it is installed, check if it is loaded
      if ("SWATprepR" %in% (.packages())) {
        # it is loaded:
        if (verbose) {
          cat(green(italic("SWATprepR already loaded!\n")))
        }
      } else{
        if (verbose){cat(green(italic("loading SWATprepR\n")))}
        # it is not loaded, then load it
        requireNamespace("SWATprepR")
      }
      # it is not installed? then require the user to install it.
    } else{
      stop(
        "SWATprepR is required for this function, please install it.
         \n https://biopsichas.github.io/SWATprepR/index.html"
      )
    }

    # get the file paths and differentiate between metadata and data
    files <- list.files(path, full.names = T)
    stations <- files[which(grepl(x = files,pattern =  "metadata.csv") == FALSE)]

    # load the metadata
    metadata <- readr::read_csv(paste0(path, "/metadata.csv"), show_col_types = F) %>%
      as.data.frame()
    metadata$Source = ""

    # Convert to prepR format: in order to use PrepR to create the SWAT weather
    # files, we need to create an excel template file in the correct format. This
    # custom read function will read and process the csv files to be in the format
    # we want them in
    custom_read <- function(filepath){
      # read
      df <- readr::read_csv(filepath, show_col_types = F)

      variables <- colnames(df)

      if("daily" %in% variables){
        df2 <- data.frame(DATE = df$daily)
      }else{stop("no date found! cannot create SWAT+ input")}

      if("min_temp" %in% variables){
        df2$TMP_MIN <- df$min_temp
        # convert to C
        df2$TMP_MIN <- df2$TMP_MIN-273.15
      }

      if("max_temp" %in% variables){
        df2$TMP_MAX<- df$max_temp
        # convert to C
        df2$TMP_MAX <- df2$TMP_MAX-273.15
      }

      if("precipitation_amount" %in% variables){
        df2$PCP <- df$precipitation_amount
      }

      if("integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time" %in% variables){
        df2$SLR <- df$integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time
        # convert to MJ
        df2$SLR <- df2$SLR/1000000
      }

      if("relative_humidity_2m" %in% variables){
        df2$RELHUM <- df$relative_humidity_2m
      }

      if("wind_speed_10m" %in% variables){
        df2$WNDSPD <- df$wind_speed_10m
      }

      if("wind_direction_10m" %in% variables){
        df2$WNDIR <- df$wind_direction_10m
      }
      return(df2)
    }

    # use the custom read function to load them into memory
    if(verbose){cat(green(italic(("loading data into memory...\n"))))}
    my_data <- purrr::map(stations, custom_read)

    if(verbose){cat(green(italic(("converting data into SWATprepR format...\n"))))}
    # add station names
    names(my_data) <- paste0("ID", c(1:length(my_data)))
    # append the metadata to the front
    stations_list <- append(list(metadata), my_data)
    # and set the name of the metadata (prepR format)
    names(stations_list)[1] <- "Stations"

    # recreating metadata format for SWATprepR
    metadata_spat <-
      sf::st_as_sf(dplyr::tibble(metadata),
                   coords = c("Long",
                              "Lat"),
                   crs = 4326)
    metadata_spat$Long <- metadata$Long
    metadata_spat$Lat <- metadata$Lat
    metadata_spat$Source = NA

    # recreating data format for SWATprepR

    # this function splits the dataframe into individual lists, and appends
    # the date column to each one in tibble form. The column name for the variable
    # at hand is not assigned here because I could not find a way to do it. It
    # is done in a later step with for loops
    data_spanner <- function(station) {
      step1 <- station %>% as.list()
      data_wrench <- function(datecol, list) {
        df <- dplyr::tibble(DATE = datecol, list)
        names(df) <- c("DATE", "replace")
        return(df)
      }

      # the date col needs to be seperated out, and then is appended to each list
      # item. It is currently in a slightly deviating format, datetime, but I
      # dont think it matters, something to check up on though
      datecol = as.POSIXct(step1$DATE)
      # we apply our custom function to everything but the date column [-1]
      step2 <- lapply(step1[-1], data_wrench, datecol = datecol)

      # here we parse out the variable names and apply them to the dataframe
      the_colnames <- names(step2)
      for (i in seq_along(the_colnames)) {
        colnames(step2[[i]]) <- c("DATE", the_colnames[i])
      }
      return(step2)
    }

    # now we need to make a list of lists
    new_list <- list()
    for (i in seq_along(my_data)) {
      # for every station we first manipulate the structure using our custom function
      modified <- data_spanner(my_data[[i]])
      # turn it into a list of tibbles
      modified <- list(modified)
      # and add it to the overarching list
      new_list <- c(new_list, modified)
      # and give it the correct names
      names(new_list) <- paste0("ID", c(1:length(new_list)))
    }

    # now we add that list to another list of metadata and data, in the final
    # swatprepR format (NIGHTMARE!)
    meteo_lst <- list(stations = metadata_spat , data = new_list)
    if(verbose){print(mapview::mapview(meteo_lst$stations))}

    # if the weather generator should be calculated and written:
    if(write_wgn){
      # calculating the weather generator
      ## !!! which station should we use here? should be a parameter.
      cat(green(italic(("creating weather generator\n"))))
      wgn <- SWATprepR::prepare_wgn(
        meteo_lst,
        TMP_MAX = meteo_lst$data$ID1$TMP_MAX,
        TMP_MIN = meteo_lst$data$ID1$TMP_MIN,
        PCP = meteo_lst$data$ID1$PCP,
        RELHUM = meteo_lst$data$ID1$RELHUM,
        WNDSPD = meteo_lst$data$ID1$WNDSPD,
        SLR = meteo_lst$data$ID1$SLR
      )
      # writing the weather gen
      if(verbose){cat(green(italic("writing weather generator to file in '", swat_setup, "'\n")))}

      write.csv(wgn$wgn_st, paste0(swat_setup,"/wgn_st.csv"), row.names = FALSE, quote = FALSE)
      write.csv(wgn$wgn_data, paste0(swat_setup,"/wgn_data.csv"), row.names = FALSE, quote = FALSE)
    }

    # writing
    if(is.null(sqlite_path) == FALSE){
      # write files and add them to project sqlite
      # TODO: consider enabling fill_missing?
      if(verbose){cat(green(italic(("adding weather stations to project SQLITE\n"))))}
      SWATprepR::add_weather(
        db_path = sqlite_path,
        meteo_lst = meteo_lst,
        wgn_lst = wgn,
        fill_missing = FALSE
      )
    }else{
      # if it is null, then write just the climate files
      if(verbose){cat(green(italic(("writing weather station files\n"))))}
      SWATprepR::prepare_climate(meteo_lst,
                                 swat_setup,
                                 period_starts = start,
                                 period_ends = end)
    }
  }

#' Generate SWAT+ weather input for any watershed in the nordics.
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
