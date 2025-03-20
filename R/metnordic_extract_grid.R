if(FALSE){
  # testing par set
  # libs
  require(crayon)
  require(ncdf4)
  require(terra)
  require(tibble)
  require(sf)
  require(mapview)
  require(dplyr)
  require(readr)
  require(lubridate)
  # pars
  verbose = TRUE
  area <- sf::read_sf("../staging_ground/test_miljotools/shp/cs10_basin.shp")
  merged_path <- "../../masters-thesis/data/macroclimate/netcdf/merged/"
  outdir <- "TEMPOUTDIRSWAT"
  mn_variables <- c("air_temperature_2m", "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time", "relative_humidity_2m", "precipitation_amount", "wind_speed_10m")
}

metnordic_extract_grid <- function(merged_path, area, mn_variables, outdir, verbose){

  # sub functions
  get_overlapping_cells <- function(merged_path, area){
    filepaths <- list.files(merged_path, pattern = "metno-", full.names = T)
    basemap <- nc_open(filepaths[1])
    basemap_lon <- ncvar_get(basemap, "lon") %>% as.vector()
    basemap_lat <- ncvar_get(basemap, "lat") %>% as.vector()
    nc_close(basemap)
    points <- tibble(lon = basemap_lon, lat = basemap_lat)
    projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    grid.sf <- st_as_sf(x = points,
                        coords = c("lon", "lat"),
                        crs = projcrs)
    myrast <- terra::rast(filepaths[1])
    grid.sf.proj <- st_transform(grid.sf, st_crs(myrast))
    area <- st_transform(area, st_crs(myrast))
    rm(myrast)
    # figure out which ones are touching the area buffer
    pnts_trans <- grid.sf.proj %>% mutate(
      intersection = as.integer(st_intersects(grid.sf.proj, area)))
    grid <- grid.sf.proj[which(pnts_trans$intersection == 1),]

    return(grid)
  }
  get_timestamps <- function(merged_path, variable){
    filepaths <- list.files(merged_path, pattern = variable, full.names = T)
    ncin <- nc_open(filepaths[1])
    ## DATE formatting
    datenumeric <-  ncdf4::ncvar_get(ncin, varid = "time")
    # Then to convert hours to seconds which are the basis for the POSIXt
    # classed objects, just multiply by 3600 = 60*60:
    # https://stackoverflow.com/a/30783581
    datetime <- as.POSIXct(datenumeric*3600,origin='1901-01-01 01:00:00',) %>% as_datetime() %>% format()
    return(datetime)
  }
  extract_grid_cells <- function(variable){
    # Grab the right file
    filepath <- list.files(merged_path, pattern = variable, full.names = T)
    # load" the raster data
    print(paste0("extracting: ", variable))
    varrast <- terra::rast(filepath)
    datamatrix <- terra::extract(varrast, grid, method = "bilinear", raw = TRUE, ID = FALSE)
    datamatrix %>% return()
  }


  # main
  dir.create(outdir)
  swatprepr_check(verbose)
  grid <- get_overlapping_cells(merged_path = merged_path, area = area)
  station_nr <- grid$geometry %>% length()
  matrix_list <- lapply(X = mn_variables,FUN =  extract_grid_cells)

  for (i in c(1:station_nr)) {

    get_timeseries <- function(matrix){

      # suffix should always be 1 for the first cell?
      variable = colnames(matrix)[1] %>% stringr::str_split("_")
      splitted <- colnames(matrix)[1] %>% stringr::str_split("_") %>% unlist()
      # remove the id
      variable = paste(splitted[1:(length(splitted)-1)], collapse = "_")
      time <- get_timestamps(merged_path, variable)
      values <- ret_mat <- matrix[i,] %>% as.vector()
      ret_df <- tibble(time, values)
      colnames(ret_df) <- c("date", variable)
      return(ret_df)
    }

    cat(paste0("\rworking on station ", i, "..."))
    varlist <- lapply(X = matrix_list, get_timeseries)
    yes <- varlist %>% purrr::reduce(full_join, by = "date")
    fp <- paste0(outdir, "/metnordic_extract_grid_", i, ".csv")
    write_csv(x = yes, file = fp)
  }

  # TODO: write metadata in SWATprepR format. Get lat and long in the for loop and write to metadata file in append mode.
  # problem: this will still need elevation metadata. This could also be bilinearly interpolated.
  # TODO 2: Actually, the bi-linear interpolation does not make any sense, since we are extracting on a grid basis anyway....
}
