#' Create SWAT+ meteo input from MetNo Reanalysis3 data (SUPERSEDED)
#'
#' **SUPERSEDED**. Note: intended use of this function is within `swatplus_metnordic()` This is now an internal function.
#'
#' @seealso [swatplus_metnordic()]
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
#' @keywords internal
#'
#' @author Moritz Shore, Svajunas Plunge
#'
#' @importFrom dplyr last nth
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom stringr str_split str_remove
#' @importFrom crayon green italic
reanalysis3_swatinput <- function(path,
                                  swat_setup,
                                  write_wgn = TRUE,
                                  start = NA,
                                  end = NA,
                                  sqlite_path = NULL,
                                  verbose = FALSE,
                                  backup = FALSE,
                                  aux_data = NULL,
                                  epsg_code = NULL,
                                  fill_missing = NULL,
                                  clean_files = TRUE) {
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

      # BANDAID: filter out the NA or 0 values in case not all were passed
      # For "AUXDATA"

      not_all_na <- function(x) any(!is.na(x))
      not_all_0 <- function(x) any(x != 0) # summing the rad of all NA gives 0 i think which is why this is needed

      df %>% select(where(not_all_na)) %>% select(where(not_all_0)) -> df_filt
      variables <- colnames(df_filt)

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
    metadata_spat$Source = "MET NORDIC DATASET"

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

    if(aux_data %>% is.null() == FALSE){
      ## Grab aux data:
      mt_print(verbose, "swatplus_metnordic", "Adding Auxiliary data..")
      aux_data_data <- SWATprepR::load_template(template_path = aux_data, epsg_code = epsg_code)
      nr_metnordic_stations <- length(new_list)
      nr_aux_stations <- length(aux_data_data$stations$ID)
      new_IDS <- paste0("ID", c((nr_metnordic_stations+1):(nr_aux_stations+nr_metnordic_stations)))
      aux_data_data$stations$ID <- new_IDS
      aux_data_data$stations <- aux_data_data$stations %>% sf::st_transform(sf::st_crs(metadata_spat))
      sf::st_coordinates(aux_data_data$stations) -> aux_coords
      aux_long <- aux_coords[,1] %>% unname()
      aux_lat <- aux_coords[,2] %>% unname()
      aux_data_data$stations$Long <- aux_long
      aux_data_data$stations$Lat <- aux_lat
      names(aux_data_data$data) <- new_IDS

      metadata_spat_aux <- rbind(metadata_spat, aux_data_data$stations)
      data_prepr_aux <- c(new_list, aux_data_data$data)
      meteo_lst <- list(stations = metadata_spat_aux, data = data_prepr_aux)
      #if(verbose){print(mapview::mapview(meteo_lst$stations))}
    }else{
      meteo_lst <- list(stations = metadata_spat , data = new_list)
    }
    # if the weather generator should be calculated and written:
    if(write_wgn){
      # calculating the weather generator
      cat(green(italic(("creating weather generator\n"))))
      wgn <- SWATprepR::prepare_wgn(meteo_lst)
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
        fill_missing = fill_missing
      )
    }else{
      # if it is null, then write just the climate files
      if(verbose){cat(green(italic(("writing weather station files\n"))))}
      SWATprepR::prepare_climate(meteo_lst = meteo_lst,
                                 write_path = swat_setup,
                                 period_starts = start,
                                 period_ends = end,
                                 clean_files = clean_files)
    }
  }


#' Apply MetNordic meteo data to a SWAT+ setup
#'
#' This function takes the output from `metnordic_extract_grid()` and applies it
#' to a SWAT+ setup, with the help of [SWATprepR](https://biopsichas.github.io/SWATprepR/).
#'
#' @seealso [metnordic_extract_grid()]
#'
#' @param directory path to weather data as created by `metnordic_extract_grid()`
#' @param swat_setup path to SWAT+ setup
#' @param write_wgn should the weather generator be written?
#' @param start optional parameter to define start date of time series
#' @param end optional parameter to define end date of time series4
#' @param sqlite_path path to your SWAT+ sqlite file (only needed if you wish to update your database). Warning: start and end parameters will be ignored in this case (SWATprepR limitation)
#' @param backup (logical, defaults to true) creates a backup of your swat folder before modification
#' @param aux_data Additional (non-metnordic) data to add to your SWAT+ project. Must be a path to an .xlsx file which matches the `SWATprepR` format of `load_template()`.
#' @param epsg_code `SWATprepR`: (optional) Integer, EPSG code for station coordinates. Default epsg_code = 4326, which stands for WGS 84 coordinate system.
#' @param fill_missing `SWATprepR`: (optional) Boolean, TRUE - fill data for missing stations with data from closest stations with available data. FALSE - leave stations without data. Weather generator will be used to fill missing variables for a model. Default fill_missing = TRUE.
#' @param clean_files `SWATprepR`: Logical, if `TRUE`, will remove all existing weather files in model setup folder before writing new ones. Default `clean_files = TRUE`.
#' @param verbose print to console?
#'
#' @returns path to SWAT+ setup
#' @export
#'
#' @importFrom dplyr summarize
#'
#'
swatplus_metnordic <- function(directory,
                               swat_setup,
                               write_wgn = TRUE,
                               start = NA,
                               end = NA,
                               sqlite_path = NULL,
                               backup = FALSE,
                               aux_data = NULL,
                               epsg_code = NULL,
                               fill_missing = TRUE,
                               clean_files = TRUE,
                               verbose = FALSE) {
  coordpair <- station <- min_air <- max_air <- air_temperature_2m <- precipitation_amount <- integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time <- wind_speed_10m <- relative_humidity_2m <-  NULL
  output <- paste0(getwd(), "/swatplus_metnordic_temp")
  mt_print(verbose, function_name = "swatplus_metnordic", text = "Creating temp directory", output)
  dir.create(output)

  # convert to daily
  mt_print(verbose, function_name = "swatplus_metnordic", text = "Loading data..")
  all_data <- list.files(directory, full.names = T, pattern = "extract_grid") %>% vroom::vroom(id = "station", show_col_types = F)
  all_data <- all_data %>% mutate(station = (station %>% str_split(pattern = "metnordic_extract_grid_", simplify = T))[,2] %>% str_remove(".csv"),
                                  day = as.Date(date))

  # summarize
  mt_print(verbose, function_name = "swatplus_metnordic", text = "Converting to daily..")

  daily_data <- all_data %>% dplyr::group_by(station, day) %>% dplyr::summarize(
    air_temperature_2m = mean(air_temperature_2m) %>% round(2),
    min_air  = min(air_temperature_2m) %>% round(2),
    max_air = max(air_temperature_2m) %>% round(2),
    relative_humidity_2m = mean(relative_humidity_2m) %>% round(2),
    wind_speed_10m = mean(wind_speed_10m) %>% round(2),
    integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time = sum(integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time) %>% round(2),
    precipitation_amount = sum(precipitation_amount) %>% round(2)
  )  %>%  dplyr::select(station, daily = day, air_temperature_2m, relative_humidity_2m, wind_speed_10m, integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time, precipitation_amount, max_temp = max_air, min_temp = min_air)

  write_indiv_station <- function(stat_id){
    data = daily_data %>% dplyr::filter(station == stat_id) %>% dplyr::ungroup() %>% dplyr::select(-station)
    filename = paste0("station_", stat_id)
    filepath = paste0(output, "/", filename, ".csv")
    readr::write_csv(x = data, file = filepath)
    return(TRUE)
  }
  mt_print(verbose, function_name = "swatplus_metnordic", text = "Writing files to", output)

  daily_data$station %>% unique() %>% as.numeric() %>% sort() %>% as.character() -> all_stations
  dump = lapply(all_stations, write_indiv_station)

  ## Write metadata
  mt_print(verbose, function_name = "swatplus_metnordic", text = "Loading metadata..")

  readmeta <- function(fp){
   s1 =  utils::read.table(sep = "=", file = fp) %>% tibble::as_tibble()
   ID = s1$V2[1] %>% stringr::str_replace(" plot", "ID")
   Name = s1$V2[1] %>% stringr::str_replace(" plot", "vstation_")
   Elevation = s1$V2[5] %>% as.numeric()
   Source = ""

   # converting to lon lat
    x = s1$V2[3]
    y = s1$V2[4]

    sf::st_as_sf(x = data.frame(x = x, y = y),
             coords = c("x", "y"),
             crs =  "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6371000") %>%
      sf::st_transform(coordpair, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
      sf::st_coordinates() -> geocoords
  Long = geocoords[1]
  Lat = geocoords[2]
  data.frame(ID = ID, Name = Name, Elevation = Elevation, Source = Source, Long = Long, Lat = Lat) %>% return()
  }

  all_meta_files <- metadata <- list.files(directory, full.names = T, pattern = "_meta_")
  lapply(all_meta_files, readmeta) -> metadf

  do.call("rbind",args = metadf) %>% tibble::as_tibble() -> final_meta

  filepath = paste0(output, "/", "metadata.csv")
  mt_print(verbose, function_name = "swatplus_metnordic", text = "Writing metadata to:", filepath)
  readr::write_csv(x = final_meta, file = filepath)

  #NEXT: do SWAP prepr
  reanalysis3_swatinput(
    path = output,
    swat_setup = swat_setup,
    write_wgn = write_wgn,
    start = start,
    end = end,
    sqlite_path = sqlite_path,
    verbose =  verbose,
    backup =  backup,
    aux_data = aux_data,
    epsg_code = epsg_code,
    fill_missing = fill_missing,
    clean_files = clean_files
  )
  mt_print(verbose, function_name = "swatplus_metnordic", text = "Finished! deleting temp directory", output)
  unlink(output, recursive = T)
  return(swat_setup)
  }
