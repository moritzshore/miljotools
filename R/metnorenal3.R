# EPGS code: 4368

# opendap protocol:
# https://thredds.met.no/thredds/dodsC/metpparchivev3/2023/01/31/met_analysis_1_0km_nordic_20230131T23Z.nc.html

# source data URL:
# https://thredds.met.no/thredds/catalog/metpparchivev3/catalog.html

# server status:
# https://status.met.no/


#' Download data from the metno renanalysis project
#'
#' This function accesses the THREDDS server of met.no with download queries
#' formatted from user input. (to be expanded upon)
#'
#' @param area (string) path to geo-referenced shapefile covering the desired area
#' @param directory (string) path to desired working directory (default: working directory)
#' @param fromdate (string) date and time for start of time series (ie. "2012-09-01 10:00:00")
#' @param todate (string) date and time for end of time series (ie. "2013-09-01 10:00:00")
#' @param area_buffer desired buffer around the provided shapefile (in meters, default 1500)
#' @param preview generate graphs showing previews of data download? (boolean)
#' @importFrom abind abind
#' @importFrom dplyr nth mutate %>%
#' @importFrom lubridate year month day hour
#' @importFrom mapview mapview
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom purrr map
#' @importFrom readr write_csv
#' @importFrom sf read_sf st_crs st_transform st_buffer st_bbox st_as_sf st_intersects st_coordinates st_zm
#' @importFrom stringr str_pad str_replace_all str_split
#'
#' @author Moritz Shore
#' @export
#' @return Writes .csv files into a folder located at the provided directory.
#'   One .csv file for each grid point within the (buffered) shape file area.
#'   Addtionally one metadata file (.csv) is written with the attributes of each
#'   other file
#'
#' @examples
#'  # for demonstration purposes, use path of package
#'  ##example_file_path <- system.file(package = "miljotools", "/extdata/metno_reanal/watershed.shp")
#'
#'  ##get_metno_reanalysis3(
#'  ##area = example_file_path,
#'  ##fromdate = "2015-01-01",
#'  ##todate = "2015-01-02",
#'  ##area_buffer = 100,
#'  ##preview = TRUE
#'  ##)
#'
#'

get_metno_reanalysis3 <-
  function(area,
           directory = NULL,
           fromdate =  "2012-09-01 10:00:00",
           todate = "2012-09-01 20:00:00",
           area_buffer = 1500,
           preview = TRUE
  ){

    if(directory %>% is.null()){
      directory <- getwd()
    }

    # load in the shape file
    area <- sf::read_sf(area)

    # drop the Z coordinate
    area <- sf::st_zm(area)

    # supporting functions ----
    nc_open_retry <- function(link) {

      nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                          error = function(cond){
                            warning("failed..")
                            return(NA)
                          })

      if(nc_file %>% length() > 1){
        return(nc_file)
      } else{
        print("retry download..")
        attempt = 1
        while((attempt < 10) & (length(nc_file) == 1)){
          Sys.sleep(5)
          attempt = attempt + 1
          nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                              error = function(cond){
                                warning("failed..", cond, "retry!")
                                return(NA)
                              })

        }

        if(length(nc_file) > 1){
          print("connection re-established!")
          return(nc_file)
        }else{
          stop("download failed after 10 attempts.")
        }
      }
    }

    get_coord_window <- function(area, area_buffer, preview){

      # lambert conform conical (the projection used by met reanalysis)
      projection <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6371000"
      proj_crs <- sf::st_crs(projection) # replace with sf::crs()

      # Transform the shapefile to the metno projection
      area <- sf::st_transform(area, crs = proj_crs)

      # Buffer the shapefile to the user defined amount
      area_buff <- sf::st_buffer(x = area, dist = area_buffer)

      # get the bounding box of this shape
      wsbox <- sf::st_bbox(area_buff)

      # Grabbing the corners
      ymin <- wsbox[["ymin"]]
      ymax <- wsbox[["ymax"]]
      xmin <- wsbox[["xmin"]]
      xmax <- wsbox[["xmax"]]

      # previewing coverage
      if(preview){
        plot <- mapview::mapview(wsbox, col.region = "blue")+
          mapview::mapview(area_buff, col.region = "red")+
          mapview::mapview(area, col.region = "orange")

        print(plot)
      }

      # get a base file to find the right x y
      filename = "https://thredds.met.no/thredds/dodsC/metpparchivev3/2023/01/31/met_analysis_1_0km_nordic_20230131T23Z.nc"
      ncin <- nc_open_retry(filename)


      x <- ncdf4::ncvar_get(ncin, "x")
      y <- ncdf4::ncvar_get(ncin, "y")

      ncdf4::nc_close(ncin)

      ## Finding the nearest neighbor to each corner
      # calculate the difference in value
      x_mn_diff <- abs(x-xmin)
      x_mx_diff <- abs(x-xmax)
      y_mn_diff <- abs(y-ymin)
      y_mx_diff <- abs(y-ymax)

      # find the minimum
      min_diff_xmin <- min(x_mn_diff)
      min_diff_xmax <- min(x_mx_diff)
      min_diff_ymin <- min(y_mn_diff)
      min_diff_ymax <- min(y_mx_diff)

      # find the index of the minimum
      index_xmin <- which(min_diff_xmin == x_mn_diff)
      index_xmax <- which(min_diff_xmax == x_mx_diff)

      index_ymin <- which(min_diff_ymin == y_mn_diff)
      index_ymax <- which(min_diff_ymax == y_mx_diff)



      return(list(index_xmin = index_xmin,
                  index_xmax = index_xmax,
                  index_ymin = index_ymin,
                  index_ymax = index_ymax,
                  area_buff = area_buff
      ))
    }


    build_query <- function(bounding_coords, swatvars, fromdate, todate){

      index_xmin = bounding_coords$index_xmin
      index_xmax = bounding_coords$index_xmax
      index_ymin = bounding_coords$index_ymin
      index_ymax = bounding_coords$index_ymax

      # from min x/y to max x/y by step of 1
      x1 = index_xmin
      x2 = index_xmax
      xstep = 1

      y1 = index_ymin
      y2 = index_ymax
      ystep = 1

      # timestep not really needed since the files are individual
      time1 = 0
      time2 = 0
      timestep = 1

      # paste together the vars
      x_q <- paste0("[", x1, ":", xstep,":", x2, "]")
      y_q <- paste0("[", y1, ":", ystep,":", y2, "]")
      time_q <- paste0("[",time1, ":", timestep,":", time2, "]")

      latitude <- paste0("latitude", y_q, x_q)
      longitude <-  paste0("longitude", y_q, x_q)
      altitude <- paste0("altitude", y_q, x_q)

      # not always available:
      # notfull <- c("integral_of_surface_downwelling_longwave_flux_in_air_wrt_time")

      # paste together the variable query
      var_q <- paste0(swatvars, time_q, y_q, x_q, collapse = ",")

      # paste together the full variable query
      var_query <-
        paste0("x",
               x_q,
               ",",
               "y",
               y_q,
               ",",
               latitude,
               ",",
               longitude,
               ",",
               altitude,
               ",",
               var_q)

      # create the daterange
      daterange <- seq(as.POSIXct(fromdate, tz = "CET"), as.POSIXct(todate, tz = "CET"), by="hour")
      years <- lubridate::year(daterange)
      months <- lubridate::month(daterange) %>% stringr::str_pad(width = 2, side = "left", pad = "0")
      days <- lubridate::day(daterange) %>% stringr::str_pad(width = 2, side = "left", pad = "0")
      hours <- lubridate::hour(daterange) %>% stringr::str_pad(width = 2, side = "left", pad = "0")

      # create the file names using the date range
      filenames <- paste0("met_analysis_1_0km_nordic_", years, months, days, "T", hours, "Z", ".nc")
      # create the thredds filepath using the date range
      filepath <- paste0(years, "/", months,"/",days,"/")

      # header for thredds server
      header = "https://thredds.met.no/thredds/dodsC/metpparchivev3/"

      # full query URL pasted together
      full_urls <- paste0(header, filepath, filenames, "?", var_query)

      return(list(
        full_urls = full_urls,
        filenames = filenames,
        years = years,
        daterange = daterange
      ))

    }


    getncvar <- function(var, ncin_crop) {
      attempt = 0
      vals_crop = NA
      while ((attempt < 10) & (is.matrix(vals_crop)==FALSE)) {
        attempt = attempt + 1
        vals_crop <- tryCatch(
          expr = {
            ncdf4::ncvar_get(ncin_crop, var)
          },
          error = function(cond) {
            message(paste("error!", cond))
            Sys.sleep(10)
            return(NA)
          }
        )
      }
      return(vals_crop)
    }

    create_download_folder <- function(directory){
      # parsing date and time
      tad <-
        Sys.time() %>% stringr::str_replace_all("-", "") %>%
        stringr::str_replace_all(":", "") %>%
        stringr::str_replace_all(" ", "") %>%
        stringr::str_split("\\.", 2) %>% unlist() %>% dplyr::nth(1)
      # creating foldername
      foldername <- paste0("met_no_dl_",tad)
      # creating directory for download
      dir.create(paste0(directory, "/", foldername))

      return(foldername)
    }

    download_ncfiles <-
      function(directory, foldername, full_urls, filenames, years, swatvars) {

        # download batches per year
        yearbatch <- split(full_urls, f = years)
        filebatch <- split(filenames, f = years)

        # set list names
        years_string <- years %>% unique() %>% sort()
        names(yearbatch) <-  paste0("y", years_string)

        for (cbyear in names(yearbatch)) {
          print(paste0("downloading: ", cbyear))
          url <- yearbatch[[cbyear]]

          ncin_crop <- nc_open_retry(url[1])
          # pre-download first frame to get dimensions set


          x_crop <- ncdf4::ncvar_get(ncin_crop,"x")
          y_crop <- ncdf4::ncvar_get(ncin_crop,"y")
          lon_crop <- ncdf4::ncvar_get(ncin_crop,"longitude")
          lat_crop <- ncdf4::ncvar_get(ncin_crop,"latitude")
          alt_crop <- ncdf4::ncvar_get(ncin_crop,"altitude")

          # download all the variables using custom function
          vardl <- lapply(swatvars, getncvar, ncin_crop = ncin_crop)

          ncdf4::nc_close(ncin_crop)

          # set colnames
          names(vardl) <- swatvars

          # predefile the master matrix
          mastermatrix <- vardl

          # repeat for all following files
          for (idate in c(2:length(url))) {
            # print status
            cat("\r","downloading files ", " (", idate, "/", length(url), ")", sep = "")

            # open Netcdf file
            ncin_crop <- nc_open_retry(url[idate])

            # download all vars
            vardl <- lapply(swatvars, getncvar, ncin_crop = ncin_crop)

            ncdf4::nc_close(ncin_crop)

            # set column names
            names(vardl) <- swatvars

            # for every variable, bind the matrix slice onto the full matrix (dimension 3 --> "along=3")
            for (variable in swatvars) {
              # if the download failed, add a full frame of NAs to stack
              if (vardl[[variable]] %>% length() == 0) {
                dims <- mastermatrix[[variable]] %>% dim()
                na_frame <-
                  matrix(data = NA,
                         nrow = dims[1],
                         ncol = dims[2])
                mastermatrix[[variable]] <-
                  abind::abind(mastermatrix[[variable]], na_frame, along = 3)
              } else{
                mastermatrix[[variable]] <-
                  abind::abind(mastermatrix[[variable]], vardl[[variable]], along = 3)
              } # end else
            } # for every var
          } # for every day

          cat("\n")
          # save to rds file
          rdsfilepath <- paste0(directory, "/", foldername, "/", cbyear, ".rds")
          saveRDS(object = mastermatrix, file = rdsfilepath)
        }
        return(
          list(
            lat_crop = lat_crop,
            lon_crop = lon_crop,
            vardl = vardl,
            x_crop = x_crop,
            y_crop = y_crop,
            alt_crop = alt_crop
          )
        )
      }

    merge_rds <- function(directory, foldername, years){
      rdsfiles <- list.files(paste0(directory, "/", foldername), full.names = T)
      rdslist <- purrr::map(rdsfiles, readRDS)

      # predef
      ycount <- years %>% unique() %>% length()
      mastermatrix <- rdslist[[1]]

      # if there is only one year, no need to merge anything
      if(rdslist %>% length() == 1){
        rm(rdslist)
        return(list(mastermatrix = mastermatrix, rdsfiles = rdsfiles))
      }

      for (cyear in c(2:ycount)) {
        cyearlist <- rdslist[[cyear]]
        for (var in names(cyearlist)) {
          mastermatrix[[var]] <-
            abind::abind(mastermatrix[[var]], cyearlist[[var]], along = 3)
        }
      }
      rm(rdslist)

      return(list(mastermatrix = mastermatrix, rdsfiles = rdsfiles))
    }


    crop_dataset <- function(lat_crop, lon_crop, area, area_buff, preview){
      ### removing non touching points
      # create DF of points
      stations = data.frame(
        lat=lat_crop %>% as.vector(),
        lon=lon_crop %>% as.vector()
      )

      # create shape file from DF
      stations = sf::st_as_sf(stations, coords = c("lon","lat"), remove = FALSE)

      # set the geographic CRS
      sf::st_crs(stations) = sf::st_crs("+init=epsg:4326")

      # lambert conform conical (the projection used by met reanalysis)
      projection <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6371000"
      proj_crs <- sf::st_crs(projection) # replace with sf::crs()

      # transform to reanalysis projection
      stations <- sf::st_transform(stations, proj_crs)

      # figure out which ones are touching the area buffer
      pnts_trans <- stations %>% dplyr::mutate(
        intersection = as.integer(sf::st_intersects(stations, area_buff)))
      cover_stations <- stations[which(pnts_trans$intersection == 1),]

      # preview plot
      if(preview){
        plot <- mapview::mapview(area)+mapview::mapview(stations)
        print(plot)
      }
      return(cover_stations)
    }


    write_stations <-
      function(vardl,
               cover_stations,
               swatvars,
               x_crop,
               y_crop,
               lon_crop,
               lat_crop,
               alt_crop,
               mastermatrix,
               daterange,
               foldername,
               rdsfiles,
               directory,
               preview,
               area) {
        # getting dimensions of the x and y grid
        x_mat <- dim(vardl[[1]])[1]
        y_mat <- dim(vardl[[1]])[2]

        # total files to write
        i = 1 # iterator of file writing
        total_files <- length(cover_stations$lat)

        # predefine metadata DF
        metadata <-
          data.frame(
            ID = NA,
            Name = NA,
            Elevation = NA,
            Source = NA,
            Long = NA,
            Lat = NA
          )

        # this rounding thing is a not a great way of doing it, but also not a bad
        # way, as the results are perfect. it might break though? consider redoing
        covercoords <- sf::st_coordinates(cover_stations) %>% as.data.frame()
        covercoordcart <- paste0("(", covercoords$X %>% round(0), ",",covercoords$Y %>% round(0), ")")

        for (xcell in c(1:x_mat)) {
          for (ycell in c(1:y_mat)) {

            # this rounding thing is a not a great way of doing it, but also not a bad
            # way, as the results are perfect. it might break though? consider redoing
            statcoords <- paste0("(", x_crop[xcell] %>% round(0), ",", y_crop[ycell] %>% round(0), ")")

            # skip writing file if no coverage exists
            if(statcoords %in% covercoordcart){
              #print("yes")
            }else{
              #print("no, skipping")
              next()
            }

            # pre define master DF
            master_df <- data.frame(date = daterange)

            # building the time series data frame column for each variable
            for (variable in swatvars) {
              # get the variable out of the list
              varslice <- mastermatrix[[variable]]
              # extract the timeseries for the given cell coordinates
              timeseries <- varslice[xcell,ycell,]
              ts_df <- data.frame(value = timeseries)
              # add the column to the master df
              master_df <- cbind(master_df, ts_df)
            }
            # get the lat-lon for the grid cell (for metadata.csv)
            # i need to double check that these are lat or lon
            clon <- lon_crop[xcell, ycell]
            clat <- lat_crop[xcell, ycell]

            # set the col names
            colnames(master_df) <- c("date", swatvars)

            # get the altitude of the cell (for metadata.csv)
            altitude <- alt_crop[xcell, ycell]

            # generate a file name
            filename = paste0("sta_x",xcell,"_y", ycell,".csv")

            # add the metadata to the master file
            metadata_row <- data.frame(ID = paste0("ID",i),
                                       Name = paste0("sta_x",xcell,"_y", ycell),
                                       Elevation = altitude,
                                       Source = "",
                                       Long = clon, Lat = clat)
            metadata <- rbind(metadata, metadata_row)

            # iterate
            i = i + 1
            # status print
            cat("\r","writing files... (", i, "/" ,total_files+1, ")", sep = "")

            # write the file
            readr::write_csv(master_df, file = paste0(directory, "/", foldername, "/", filename))
          }
        }

        # write the metadata
        cat("\nwriting metadata..")
        readr::write_csv(metadata[-1,], file = paste0(directory, "/", foldername, "/metadata.csv"))

        # status print
        cat("\nfinished! files have been written to: \n", paste0(directory, "/",foldername, " "), sep = "")

        # plot of cropped stations
        if(preview){
          plot <- mapview::mapview(area, col.region = "orange")+mapview::mapview(cover_stations, cex = 3)
          print(plot)
        }

        # remove RDS files
        stat <- file.remove(rdsfiles)
      }


    ### START MAIN FUNCTION ----

    # Add stop if nots (date wrong order)

    # all relevant variables (ADD TO FUNCTION PARAMS?)
    swatvars <- c(
      "air_temperature_2m",
      "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",
      "relative_humidity_2m",
      "precipitation_amount",
      "wind_speed_10m",
      "wind_direction_10m"
    )

    print("getting coordinates..")
    bounding_coords <- get_coord_window(area, area_buffer, preview)

    print("building query..")
    queries <- build_query(bounding_coords, swatvars, fromdate, todate)

    print("creating download folder..")
    foldername <- create_download_folder(directory)

    print("starting download")
    ncdownload <-
      download_ncfiles(
        directory = directory,
        foldername = foldername,
        full_urls = queries$full_urls,
        filenames = queries$filenames,
        years = queries$years,
        swatvars = swatvars
      )

    print("download complete!, merging files..")

    merged_data <- merge_rds(directory = directory,
                              foldername = foldername,
                              years = queries$years)

    print("cropping dataset to area coverage..")
    cover_stations <-
      crop_dataset(
        lat_crop = ncdownload$lat_crop,
        lon_crop = ncdownload$lon_crop,
        area = area,
        area_buff = bounding_coords$area_buff,
        preview = preview
      )

    print("writing station data to csv..")
    write_stations(
      vardl = ncdownload$vardl,
      cover_stations = cover_stations,
      swatvars = swatvars,
      x_crop = ncdownload$x_crop,
      y_crop = ncdownload$y_crop,
      lon_crop = ncdownload$lon_crop,
      lat_crop = ncdownload$lat_crop,
      alt_crop = ncdownload$alt_crop,
      mastermatrix = merged_data$mastermatrix,
      daterange = queries$daterange,
      foldername = foldername,
      directory = directory,
      rdsfiles = merged_data$rdsfiles,
      preview = preview,
      area = area
    )

    return(paste0(directory, "/",foldername))
  }


### Downscale resolution
#' Downscale MetNo Reanalysis3 data to daily resolution
#'
#' This function takes the hourly data from get_metno_reanalysis3() and
#' recalculates it into daily time resolution.
#'
#' Warning, this funcitonality requires "svatools" to be installed:
#'
#' https://github.com/biopsichas/svatools
#'
#' @param path path to output of get_metno_reanalysis3()
#' @param outpath (optional) path to write the new files to
#' @param verbose (optional) flag to print status
#' @param precision round the values to what precision? (integrer, default 2)
#'
#' @return path of written files
#'
#' @export
#'
#' @importFrom crayon underline italic blue green
#' @importFrom dplyr %>% group_by all_of across summarise last
#' @importFrom purrr map
#' @importFrom readr read_csv write_csv
#' @importFrom lubridate date
#' @importFrom stringr str_split
reanalysis3_daily <- function(path, outpath = NULL, verbose = FALSE, precision = 2){

  #path <- "C:/Users/mosh/Documents/met_no_dl_20231020191332"
  if(verbose){cat(green(italic("reading files..\n")))}

  # get the file paths and differentiate between metadata and data
  files <- list.files(path, full.names = T)
  files_short <- list.files(path, full.names = F)
  stations_short <- files_short[which(grepl(x = files,pattern =  "metadata.csv") == FALSE)]

  stations <- files[which(grepl(x = files,pattern =  "metadata.csv") == FALSE)]
  metadata <- files[which(grepl(x = files,pattern =  "metadata.csv"))]

  # Custom funciton to convert the hourly data to daily data
  # certain considerations need to be made when summing or averaging.
  hourly2daily <- function(data){
    # These will potentially need to be expanded
    sum_these <- "precipitation_amount"
    max_these <- "air_temperature_2m"
    min_these <- "air_temperature_2m"

    data_cols <- colnames(data)[2:length(colnames(data))]
    mean_data_cols <- data_cols[which((data_cols %in% sum_these) == FALSE)]
    sum_data_cols <- data_cols[which(data_cols %in% sum_these)]
    max_data_cols <- data_cols[which(data_cols %in% max_these)]
    min_data_cols <- data_cols[which(data_cols %in% min_these)]


    data$daily <- data$date %>% lubridate::date()

    daily_data <- data %>% group_by(daily) %>%
      summarise(across(all_of(mean_data_cols), mean))

    daily_data_sum <- data %>% group_by(daily) %>%
      summarise(across(all_of(sum_data_cols), sum))

    daily_data_max <- data %>% group_by(daily) %>%
      summarise(across(all_of(max_data_cols), max)) %>%
      rename(max_temp = air_temperature_2m)

    daily_data_min <- data %>% group_by(daily) %>%
      summarise(across(all_of(min_data_cols), min)) %>%
      rename(min_temp = air_temperature_2m)

    # -1 to get rid of the date column and round by precisision
    full_df <-
      cbind(
        daily_data[1],
        daily_data[-1] %>% round(precision),
        daily_data_sum[-1] %>% round(precision),
        daily_data_max[-1] %>% round(precision),
        daily_data_min[-1] %>% round(precision)
      )

    return(full_df)
  }

  # load all the csv files into memory
  if(verbose){cat(green(italic("loading files into memory..\n")))}

  data_frames <- purrr::map(stations, readr::read_csv, show_col_types = F)
  # process them with custom function
  if(verbose){cat(green(italic("re-calculating to daily..\n")))}

  daily_data_frames <- purrr::map(data_frames, hourly2daily)
  # write them to custom folder

  foldername <- stringr::str_split(path, "/", simplify = T) %>%
    as.vector() %>% last() %>% paste0("_daily")

  if(outpath %>% is.null()){
    outpath = paste0(path, "_daily")
  }else{
    outpath <- paste0(outpath, "/", foldername)
  }

  if(verbose){cat(green(italic("copying metadata..\n")))}


  out_filepaths <- paste0(outpath, "/", stations_short)
  dir.create(outpath)
  quiet <- file.copy(from = metadata, to = paste0(outpath, "/metadata.csv"))

  if(verbose){cat(green(italic("writing files..\n")))}

  for (i in 1:length(daily_data_frames)) {
    readr::write_csv(daily_data_frames[[i]], file = out_filepaths[i], progress = F)
  }

  if(verbose){
    cat("Conversion finished, files written to:\n", underline(blue(outpath)), "\n")
  }

  return(outpath)
}


#' Create SWAT+ meteo input from MetNo Reanalysis3 data
#'
#' Takes data gathered by `get_metno_reanalysis3()` and downscaled by
#' `reanalysis3_daily()` and creates SWAT+ meteo input files and weather
#' generators.
#'
#' @param path path to daily data provided by `reanalysis3_daily()`
#' @param sqlite_path path to your SWAT+ sqlite file.
#' @param outpath optional path to directory where .xlsx file will be written.
#' @param verbose print status?
#'
#' @return path to generated files.
#' @export
#'
#' @importFrom dplyr last nth
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom stringr str_split str_remove
#' @importFrom writexl write_xlsx
reanalysis3_swatinput <- function(path, sqlite_path, outpath = NULL, verbose = FALSE){

  # Check that svatools is installed
  if ("svatools" %in% utils::installed.packages()) {
    # nothing to do
  } else{
    cat("svatools is required for this functionality, would you like to install? (y) or not (n) \n")
    answer <- readline()

    if(answer != "y"){return(FALSE)}

    devtools::install_github("biopsichas/svatools")
    if ("svatools" %in% utils::installed.packages()) {
      underline(blue(cat("svatools installed succesfully\n")))
    } else{
      stop("svatools install unsuccessful! \nYou can try installing the package manually from github.")
    }
  }

  # get the file paths and differentiate between metadata and data
  files <- list.files(path, full.names = T)
  files_short <- list.files(path, full.names = F)
  stations_short <- files_short[which(grepl(x = files,pattern =  "metadata.csv") == FALSE)]
  stations <- files[which(grepl(x = files,pattern =  "metadata.csv") == FALSE)]

  # load the metadata
  metadata <- readr::read_csv(paste0(path, "/metadata.csv"), show_col_types = F) %>%
    as.data.frame()
  metadata$Source = ""


  # Convert to svatools format

  # in order to use svatools to create the SWAT weather files, we need to create
  # an excel template file in the correct format.

  # This custom read function will read and process the csv files to be in the
  # format we want them in
  custom_read <- function(filepath){
    # read
    df <- readr::read_csv(filepath, show_col_types = F)

    variables <- colnames(df)

    if("daily" %in% variables){
      df <- df %>% rename(DATE = daily)
    }else{stop("no date found! cannot create SWAT+ input")}

    if("min_temp" %in% variables){
      df <- df %>% rename(TMP_MIN = min_temp)
      # convert to C
      df$TMP_MIN <- df$TMP_MIN-273.15
    }

    if("max_temp" %in% variables){
      df <- df %>% rename(TMP_MAX = max_temp)
      # convert to C
      df$TMP_MAX <- df$TMP_MAX-273.15
    }

    if("precipitation_amount" %in% variables){
      df <- df %>% rename(PCP = precipitation_amount)
    }

    if("integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time" %in% variables){
      df <- df %>% rename(SLR = integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time)

      # convert to MJ
      df$SLR <- df$SLR/1000000
    }

    if("relative_humidity_2m" %in% variables){
      df <- df %>% rename(RELHUM = relative_humidity_2m)
    }

    if("wind_speed_10m" %in% variables){
      df <- df %>% rename(WNDSPD = wind_speed_10m)
    }

    if("wind_direction_10m" %in% variables){
      df <- df %>% rename(WNDIR = wind_direction_10m)
    }

    # drop average air temp since SWAT does not use it.
    df <- df %>% select(-air_temperature_2m)

    return(df)
  }

  # loading data into memory
  cat(green(italic(("loading data into memory...\n"))))
  # load all but the metadata
  # use the custom read function to load them into memory

  my_data <- purrr::map(stations, custom_read)
  # add names

  names(my_data) <- paste0("ID", c(1:length(my_data)))

  # append the metadata to the front

  stations_list <- append(list(metadata), my_data)
  # set the name of the metadata (svatools format)
  names(stations_list)[1] <- "Stations"

  folder <- path %>% stringr::str_split("/") %>% unlist() %>% dplyr::last()

  # time and date should always be the 4th element.
  tod <- folder %>% str_split("_") %>% unlist() %>% dplyr::nth(4)

  if(outpath %>% is.null()){
    outpath <- path %>% stringr::str_remove(folder)
    }

  xlpath <- paste0(outpath, "/", tod, "_swat_weather_data.xlsx")

  # writing the excel sheet
  if(verbose){cat(green(italic(("writing svatools excel sheet\n"))))}
  writexl::write_xlsx(
    x = stations_list,
    path = xlpath,
    col_names = T
  )
  # remove the big dataframes
  rm(stations_list, my_data); gc()


  # running svatool
  # loading the template
  if(verbose){cat(green(italic("loading data into svatools\n")))}
  met_lst2 <- svatools::load_template(xlpath, epsg_code = 4368)

  # calculating the weather generator
  ## !!! which station should we use here?
  cat(green(italic(("creating weather generator\n"))))
  wgn <- svatools::prepare_wgn(met_lst2,
                     TMP_MAX = met_lst$data$ID1$TMP_MAX,
                     TMP_MIN = met_lst$data$ID1$TMP_MIN,
                     PCP = met_lst$data$ID1$PCP,
                     RELHUM = met_lst$data$ID1$RELHUM,
                     WNDSPD = met_lst$data$ID1$WNDSPD,
                     SLR = met_lst$data$ID1$SLR)
  # writing the weather gen
  if(verbose){cat(green(italic("writing weather generator to file in '", outpath, "'\n")))}

  write.csv(wgn$wgn_st, paste0(outpath,"/wgn_st.csv"), row.names = FALSE, quote = FALSE)
  write.csv(wgn$wgn_data, paste0(outpath,"/wgn_data.csv"), row.names = FALSE, quote = FALSE)

  # write files and add them to project sqlite
  if(verbose){cat(green(italic(("adding weather stations to project SQLITE\n"))))}
  svatools::add_weather(sqlite_path, met_lst2, wgn)
}
