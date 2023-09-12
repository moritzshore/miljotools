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
#' @importFrom sf read_sf st_crs st_transform st_buffer st_bbox st_as_sf st_intersects st_coordinates
#' @importFrom stringr str_pad str_replace_all str_split
#'
#' @author Moritz Shore
#' @export
#' @return Writes .csv files into a folder located at the provided directory.
#'   One .csv file for each grid point within the (buffered) shape file area.
#'   Addtionally one metadata file (.csv) is written with the attributes of each
#'   other file
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