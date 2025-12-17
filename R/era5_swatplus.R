#' Convert ERA5 data from open meteo to SWAT+ input.
#'
#' `open_meteo_path` needs to point to a file downloaded from open-meteo.
#'
#' @param open_meteo_path path to file downloaded from open-meteo in daily format with the correct variables
#' @param extract_path path to location where data can be extracted and processed
#' @param swat_setup path to swat+ setup
#' @param aux_data if auxilliary data is to be added, path to xlsx file using `SWATprepR` formatting.
#' @param epsg_code See `SWATprepR` docs
#' @param verbose Print? default is `TRUE`
#' @param selected_vars Variables to incorporate from ERA5. default is all of them
#' @param write_wgn See `SWATprepR`docs
#' @param clean_files See `SWATprepR`docs
#' @param sqlite_path See `SWATprepR`docs
#' @param fill_missing See `SWATprepR` docs
#'
#' @returns Nothing
#' @export
#'
era5_swatplus <- function(open_meteo_path,
                          extract_path,
                          swat_setup,
                          aux_data = NULL,
                          epsg_code = NULL,
                          verbose = TRUE,
                          selected_vars = c("max_temp", "min_temp", "precipitation_amount", "rad", "windspeed", "rh"),
                          write_wgn = TRUE,
                          clean_files = TRUE,
                          sqlite_path = NULL,
                          fill_missing = FALSE
                          ) {
  # reset dir
  if(dir.exists(extract_path)){
    unlink(extract_path, recursive = T)
    dir.create(extract_path, showWarnings = F)
  }

  # reading in file to detect when the metadata ends and data starts
  readLines(open_meteo_path) -> mylines
  grepl(pattern = "location_id,time", x = mylines) %>% which() -> startidx # this location marks the split between metadata and data
  readr::read_csv(open_meteo_path, skip = startidx-1, show_col_types = F) -> openmeteoin
  readr::read_csv(open_meteo_path, n_max = startidx-3, show_col_types = F) -> metadata

  # determining unique grid cells and only keeping those (safeguard against
  # duplicate data). This code is not necessary if you do not download any
  # duplicate locations.
  metadata %>% select(latitude, longitude) %>% distinct() -> unique
  left_join(unique, metadata, by = c("latitude", "longitude"), multiple = "first") %>% pull(location_id) -> unique_data

  # filter and transform metadata to match SWATprepR format
  metadata %>% filter(location_id %in% unique_data) %>%
    mutate(x = latitude, y = longitude) %>%
    st_as_sf(coords = c("y", "x"), crs = st_crs(4326)) -> metadata_filt
  metadata_filt %>% mutate(ID = paste0("ID",location_id+1),
                           Name = paste0("vstat", location_id+1),
                           Elevation = elevation,
                           Source = "ERA5",
                           Long = longitude,
                           Lat = latitude) %>%
    select(-location_id, -latitude, -longitude, -elevation, -utc_offset_seconds, -timezone, -timezone_abbreviation) -> metadata_spat
  metadata_spat %>% select(ID, Name = Name, Elevation, Source, Long, Lat, geometry) -> metadata_spat

  # Filter and rename
  openmeteoin %>% filter(location_id %in% unique_data) %>%
    rename(date = time,
           min_temp = `temperature_2m_min (°C)`,
           max_temp = `temperature_2m_max (°C)`,
           precipitation_amount = `precipitation_sum (mm)`,
           rh = `relative_humidity_2m_mean (%)`,
           windspeed = `wind_speed_10m_mean (m/s)`,
           rad = `shortwave_radiation_sum (MJ/m²)`
    )  -> openmeteofilt

  # write to disk
  for (location in openmeteofilt$location_id %>% unique()) {
    print(paste0("Writing station ", location+1, " to disk"))
    dir.create(extract_path, showWarnings = F)
    fn = paste0("ERA5_location_id_",location+1, ".csv")
    fpfn = paste0(extract_path, fn)
    openmeteofilt %>% filter(location_id == location) %>% select(-location_id) %>% write_csv(file = fpfn)
  }

  custom_read <- function(filepath){
    # read
    df <- readr::read_csv(filepath, show_col_types = F)

    # BANDAID: filter out the NA or 0 values in case not all were passed
    # For "AUXDATA"
    not_all_na <- function(x) any(!is.na(x))
    not_all_0 <- function(x) any(x != 0) # summing the rad of all NA gives 0 i think which is why this is needed

    df %>% dplyr::select(dplyr::where(not_all_na)) %>% select(dplyr::where(not_all_0)) -> df_filt
    variables <- colnames(df_filt)
    variables[variables %in% c("date", selected_vars)] -> variables

    if("date" %in% variables){
      df2 <- data.frame(DATE = df$date)
    }else{stop("no date found! cannot create SWAT+ input")}

    if("min_temp" %in% variables){
      df2$TMP_MIN <- df$min_temp
    }

    if("max_temp" %in% variables){
      df2$TMP_MAX<- df$max_temp
    }

    if("precipitation_amount" %in% variables){
      df2$PCP <- df$precipitation_amount
    }

    if("rad" %in% variables){
      # convert to MJ
      df2$SLR <- df$rad
    }

    if("rh" %in% variables){
      df2$RELHUM <- df$rh/100
    }

    if("windspeed" %in% variables){
      df2$WNDSPD <- df$windspeed
    }

    if("wind_direction_10m" %in% variables){
      df2$WNDIR <- df$wind_direction_10m
    }
    return(df2)
  }

  mt_print(verbose, "era5_swatplus", "loading data into memeory..")
  stations = list.files(extract_path, full.names = T)
  my_data <- purrr::map(stations, custom_read)

  mt_print(verbose, "era5_swatplus", "converting data into SWATprepR format...")
  # add station names
  names(my_data) <- paste0("ID", c(1:length(my_data)))
  # append the metadata to the front
  stations_list <- append(list(metadata), my_data)
  # and set the name of the metadata (prepR format)
  names(stations_list)[1] <- "Stations"

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
    datecol = step1$DATE
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
    mt_print(verbose, "era5_swatplus", "Adding Auxiliary data..")
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
    mt_print(verbose, "era5_swatplus", "Creating WGN..")
    wgn <- SWATprepR::prepare_wgn(meteo_lst = meteo_lst)
  }

  start = meteo_lst$data$ID1$PCP$DATE %>% min()
  end = meteo_lst$data$ID1$PCP$DATE %>% max()

  # writing
  if(is.null(sqlite_path) == FALSE){
    # write files and add them to project sqlite
    # TODO: consider enabling fill_missing?
    mt_print(verbose, "era5_swatplus", "adding weather stations to project SQLITE..")
    SWATprepR::add_weather(
      db_path = sqlite_path,
      meteo_lst = meteo_lst,
      wgn_lst = wgn,
      fill_missing = fill_missing
    )
  }else{
    # if it is null, then write just the climate files
    mt_print(verbose, "era5_swatplus", "writing weather station files..")
    SWATprepR::prepare_climate(meteo_lst = meteo_lst,
                               write_path = swat_setup,
                               period_starts = start,
                               period_ends = end,
                               clean_files = clean_files)
  }
  mt_print(verbose, "era5_swatplus", "Done!")
}
