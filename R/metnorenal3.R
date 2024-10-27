# EPGS code: 4368

# opendap protocol:
# https://thredds.met.no/thredds/dodsC/metpparchivev3/2023/01/31/met_analysis_1_0km_nordic_20230131T23Z.nc.html

# source data URL:
# https://thredds.met.no/thredds/catalog/metpparchivev3/catalog.html

# server status:
# https://status.met.no/


#' Download data from the MetNordic Re-Analysis v3 dataset
#'
#' The MET Nordic rerun archive version 3 can be accessed using this
#' function. Please see below for more details.
#'
#' For instructions on how to use this function, you can visit the article on
#' the [web page](https://moritzshore.github.io/miljotools/articles/metno_reanal.html).
#'
#' The [MET Nordic Reanalysis Dataset](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset)
#' is a reanalysis product from the Meteorologisk institutt (hourly from 2012-09 to
#' 2023-01). Please inform yourself on the limitations of reanalysis data before
#' applying this dataset to your needs.This function accesses the THREDDS server
#' of met.no with download queries formatted from user input:
#'
#' `area` should be a path to a **geo-referenced** shapefile (of either a
#' **polygon** or a **point** geometry) of the area you would like data from.
#' This area must be at least twice the size of the chosen `grid_resolution`,
#' which is by default 1 x 1 km. If a point shapefile is given, then the nearest
#' grid cell is chosen, and data is downloaded from there.
#'
#' `directory` is an optional parameter which lets you chose in which directory
#' the downloads will be stored. If this parameter is not defined, the current
#' working directory will be used.
#'
#' `fromdate` and `todate` are the date and time by which to bound the download
#' by. required format is: **"2012-09-01 10:00:00"**. The dataset spans from this
#' date til 2023-01.
#'
#' `mn_variables` are the MetNordic Variables which the function should
#' download, by default this includes the following:
#'
#' - "air_temperature_2m",
#' - "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",
#' - "relative_humidity_2m",
#' - "precipitation_amount",
#' - "wind_speed_10m",
#' - "wind_direction_10m"
#'
#' There are a few more variables which are not downloaded, these have not been
#' tested, and some are known to have data gaps which the function currently
#' cannot deal with, and might cause it to fail. use at own risk, and post an
#' issue if anything does not work.
#'
#' You can read more about these variables on [GitHub (MetNo)](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset)
#'
#' `area_buffer` is an optional convienient parameter in which you can  buffer
#' your polygon shape file by the given amount of meters. This is useful for
#' downloading stations that are outside of the bounds of your polygon, but are
#' still relatively close to the border (due to the 1x1km grid). A good (and
#' default) value for this is 1500 m
#'
#' `grid_resolution` lets you choose how many stations to select from the grid.
#' If for example you pass a "3", then every 3 station will be selected in both
#' the horizontal and vertical axis, giving you a grid with stations placed 3km
#' apart from each other on a regular grid. This can help with downloading data
#' from huge catchments where 1x1km grid size is not necessary. Please note, no
#' averaging of the "non-selected" grid cells is performed, and the default
#' value is 1 x 1 km.
#'
#' `preview` prints information to the console as to what the function is doing,
#' as well as plotting interactive maps to the viewer which gives you an idea of
#' which grid cells were selected relative to the shapefile you provided.
#'
#' @param area (string) path to geo-referenced shapefile (polygon or point) of the desired area
#' @param directory (string) path to desired working directory (default: working directory)
#' @param fromdate (string) date and time for start of time series (ie. "2012-09-01 10:00:00")
#' @param todate (string) date and time for end of time series (ie. "2013-09-01 10:00:00")
#' @param mn_variables (vector) Leave blank for deafault (tested) variables. See details for more
#' @param area_buffer desired buffer around the provided shapefile (in meters, default 1500)
#' @param grid_resolution (integer) desired resolution of downloaded grid in kilometers. (see help page for more details)
#' @param preview generate graphs showing previews of data download? (boolean)
#' @param ncdf Set this parameter to be `TRUE` if you would like the downloaded files to remain in NCDF format (*.nc).
#' @param continue if `ncdf` is `TRUE`, then you can pass a folder name (in passed directory) here to continue an aborted download session.
#' @importFrom abind abind
#' @importFrom dplyr nth mutate %>% tibble
#' @importFrom lubridate year month day hour as_datetime
#' @importFrom mapview mapview
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom purrr map
#' @importFrom readr write_csv
#' @importFrom sf read_sf st_crs st_transform st_buffer st_bbox st_as_sf st_intersects st_coordinates st_zm
#' @importFrom stringr str_pad str_replace_all str_split
#' @importFrom mapview mapview
#' @importFrom crayon black bold green italic yellow blue
#' @importFrom utils packageVersion
#'
#' @author Moritz Shore
#' @export
#' @return Function returns a path to where .csv files of the download were
#'   written. One .csv file for each grid point within the (buffered) shape
#'   file area. Additionally one metadata file (.csv) is written with the
#'   attributes of each other file
#'
#' @examples
#'
#'  if(FALSE){
#'  get_metno_reanalysis3(
#'  area = example_file_path,
#'  fromdate = "2015-01-01",
#'  todate = "2015-01-02",
#'  area_buffer = 100,
#'  preview = TRUE
#'  )
#'  }
#'
#'

get_metno_reanalysis3 <-
  function(area,
           directory = NULL,
           fromdate =  "2012-09-01 10:00:00",
           todate = "2012-09-01 20:00:00",
           mn_variables = NULL,
           area_buffer = 1500,
           grid_resolution = NULL,
           preview = TRUE,
           ncdf = FALSE,
           continue = NULL
  ){

    get_coord_window <- function(area_path, area_buffer, preview){

      # get a base file to find the right x y
      filename = "https://thredds.met.no/thredds/dodsC/metpparchivev3/2023/01/31/met_analysis_1_0km_nordic_20230131T23Z.nc"
      ncin <- nc_open_retry(filename)


      x <- ncdf4::ncvar_get(ncin, "x")
      y <- ncdf4::ncvar_get(ncin, "y")

      ncdf4::nc_close(ncin)

      # lambert conform conical (the projection used by met reanalysis)
      projection <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6371000"
      proj_crs <- sf::st_crs(projection) # replace with sf::crs()
      # load in the shape file
      area <- sf::read_sf(area_path)
      # Transform the shapefile to the metno projection
      area <- sf::st_transform(area, crs = proj_crs)

      # get the geometry type
      area_attr <- sf::st_geometry(area) %>% attr("class") %>% nth(1)

      if (area_attr == "sfc_POINT") {
        coordinate <- sf::st_coordinates(area)
        point_x <- coordinate[1]
        point_y <- coordinate[2]

        ## Finding the nearest neighbor to each corner
        # calculate the difference in value
        x_diff <- abs(x-point_x)
        y_diff <- abs(y-point_y)

        # find the minimum
        min_diff_x <- min(x_diff)
        min_diff_y <- min(y_diff)

        # find the index of the minimum
        index_x <- which(min_diff_x == x_diff)
        index_y <- which(min_diff_y == y_diff)

        if(preview){
          df <- sf::st_as_sf(x = data.frame(x = x[index_x], y = y[index_y]),
                         coords = c("x", "y"),
                         crs = sf::st_crs(proj_crs))
          area$name = "provided file"
          plot = mapview::mapview(df, layer.name = "Nearest Re-analysis Gridpoint", col.regions = "orange")+
            mapview::mapview(area, layer.name = "User location", col.regions = "blue")

          print(plot)

          cat("Note: Selected grid cell is ")
          sf::st_distance(df, area) %>% round(0) %>% cat("meters away from desired location \n")
        }


        return(list(index_x = index_x, index_y = index_y))

      } else{
        # do the polygon stuff
        if (area_attr != "sfc_POLYGON"){warning("Shapefile type not 'sfc_POLYGON', problems may occur..")}
        # drop the Z coordinate
        area <- sf::st_zm(area)
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
                    area_buff = area_buff, area_shp = area
        ))
      }
    }

    build_query <- function(bounding_coords, mn_variables, fromdate, todate,
                            grid_resolution, verbose){

      # time step not really needed since the files are individual
      time1 = 0
      time2 = 0
      timestep = 1

      if(length(bounding_coords) == 2){
        # do point routine
        x = bounding_coords$index_x
        y = bounding_coords$index_y

        x1 = x
        x2 = x
        y1 = y
        y2 = y
        xstep = 1
        ystep = 1

      }else{
        # do polygon routine
        index_xmin = bounding_coords$index_xmin
        index_xmax = bounding_coords$index_xmax
        index_ymin = bounding_coords$index_ymin
        index_ymax = bounding_coords$index_ymax

        # checking if the grid resolution is small enough to at least download 1
        # station.
        bbox_width = index_xmax - index_xmin
        bbox_height = index_ymax - index_ymin
        if(bbox_width < (2*grid_resolution)-1){stop("Area is not big enough (too narrow) for the given grid resolution. Please use a finer resolution")}
        if(bbox_height < (2*grid_resolution)-1){stop("Area is not big enough (too short) for the given grid resolution. Please use a finer resolution")}

        # from min x/y to max x/y by step of 1
        x1 = index_xmin
        x2 = index_xmax
        xstep = grid_resolution

        y1 = index_ymin
        y2 = index_ymax
        ystep = grid_resolution

        # print grid resolution
        if(verbose){cat(green(italic("fetching with grid size of", black(bold(xstep)), "x", black(bold(ystep)), "km \n")))}
        }

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
      var_q <- paste0(mn_variables, time_q, y_q, x_q, collapse = ",")

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
      daterange <- seq(as_datetime(fromdate), as_datetime(todate), by="hour")
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

    download_ncfiles <- function(directory, foldername, full_urls, filenames,
                                 years, mn_variables, geometry_type,
                                 ncdf = FALSE, verbose = FALSE) {

      ### This is where the switch to netcdf download should take place
      ### if the user opts for it! (ncdf4)
      if(ncdf){
        savefiles = paste(directory, foldername, filenames, sep = "/")
        read_write_ncdf(url = full_urls, savefiles = savefiles,
                        foldername = foldername, directory = directory,
                        verbose = preview)
        return(directory)
      }

      # else: continue as normal
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
          vardl <- lapply(mn_variables, getncvar, ncin_crop = ncin_crop)

          ncdf4::nc_close(ncin_crop)

          # set colnames
          names(vardl) <- mn_variables

          # predefile the master matrix
          mastermatrix <- vardl

          # repeat for all following files
          for (idate in c(2:length(url))) {
            # print status
            cat("\r","downloading files ", " (", idate, "/", length(url), ")", sep = "")

            # open Netcdf file
            ncin_crop <- nc_open_retry(url[idate])

            # download all vars
            vardl <- lapply(mn_variables, getncvar, ncin_crop = ncin_crop)

            ncdf4::nc_close(ncin_crop)

            # set column names
            names(vardl) <- mn_variables

            if(geometry_type == "point"){
              mat_dimension = 2

            }
            if(geometry_type == "polygon"){
              mat_dimension = 3
            }
            # for every variable, bind the matrix slice onto the full matrix (dimension 3 --> "along=3")
            for (variable in mn_variables) {
              # if the download failed, add a full frame of NAs to stack
              if (vardl[[variable]] %>% length() == 0) {
                dims <- mastermatrix[[variable]] %>% dim()
                na_frame <-
                  matrix(data = NA,
                         nrow = dims[1],
                         ncol = dims[2])
                mastermatrix[[variable]] <-
                  abind::abind(mastermatrix[[variable]], na_frame, along = mat_dimension)
              } else{
                mastermatrix[[variable]] <-
                  abind::abind(mastermatrix[[variable]], vardl[[variable]], along = mat_dimension)
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

    write_stations <- function(vardl, cover_stations, mn_variables, x_crop, y_crop,
                               lon_crop, lat_crop, alt_crop, mastermatrix,
                               daterange, foldername, rdsfiles, directory,
                               preview, area) {
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
        # way, as the results are near perfect. it might break though? consider redoing
        covercoords <- sf::st_coordinates(cover_stations) %>% as.data.frame()
        covercoordcart <- paste0("(", covercoords$X %>% round(0), ",",covercoords$Y %>% round(0), ")")

        for (xcell in c(1:x_mat)) {
          for (ycell in c(1:y_mat)) {

            # this rounding thing is a not a great way of doing it, but also not a bad
            # way, as the results are near perfect. it might break though? consider redoing
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
            for (variable in mn_variables) {
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
            colnames(master_df) <- c("date", mn_variables)

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
        cat("\nfinished! files have been written to: \n", paste0(directory, "/",foldername, " \n"), sep = "")

        # plot of cropped stations
        if(preview){
          plot <- mapview::mapview(area, col.region = "orange")+mapview::mapview(cover_stations, cex = 3)
          print(plot)
        }

        # remove RDS files
        stat <- file.remove(rdsfiles)
    }




    ### START MAIN FUNCTION ----

    # validates input

    if(is.null(grid_resolution)){
      cat(bold("MILJOTOOLS >>"), italic(yellow("'grid_resolution' not chosen, defaulting to 1x1km grid... \n")))
      grid_resolution = 1
    }else{
      if(grid_resolution-floor(grid_resolution)!=0){stop("'grid_resolution' must be an integer!")}
      if(grid_resolution < 1){stop("`grid_resolution` must be greater than 1 km")}
    }

    # this is truly crap, should fix..
    if(preview == TRUE){verbose = TRUE}else{verbose = FALSE}

    if(directory %>% is.null()){
      directory <- getwd()
    }

    if(area_buffer < 1){
      area_buffer = 1
    }

    if(mn_variables %>% is.null()){
      # all relevant variables and tested variables
      mn_variables <- c(
        "air_temperature_2m",
        "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",
        "relative_humidity_2m",
        "precipitation_amount",
        "wind_speed_10m",
        "wind_direction_10m"
      )
      if(preview){
        cat(bold("downloading default variables:\n"))
        cat(blue(italic(mn_variables)), sep = "\n")
      }
    }else{
      cat(bold("downloading custom variables:\n"))
      cat(blue(italic(mn_variables)), sep = "\n")
    }


    print("getting coordinates..")
    bounding_coords <- get_coord_window(area_path = area, area_buffer, preview)

    print("building query..")
    queries <- build_query(bounding_coords, mn_variables, fromdate, todate, grid_resolution, verbose)

    if(is.null(continue) == FALSE){
      print("continuing download folder..")
      foldername = continue
    }else{
      print("creating download folder..")
      foldername <- create_download_folder(directory)

    }

    print("starting download")
    if(bounding_coords %>% length() == 2){geometry_type = "point"}else{geometry_type = "polygon"}

    ncdownload <-
      download_ncfiles(
        directory = directory,
        foldername = foldername,
        full_urls = queries$full_urls,
        filenames = queries$filenames,
        years = queries$years,
        mn_variables = mn_variables,
        geometry_type = geometry_type, ncdf = ncdf, verbose = preview
      )

    if(ncdf){
      if(preview){cat(bold(green("NCDF files finished downloading and are located here:")), "\n",
                      blue(italic(underline(paste0(directory, "/",foldername)))), "\n")}
      return(paste0(directory, "/",foldername))
    }
    print("download complete!, merging files..")

    merged_data <- merge_rds(directory = directory,
                              foldername = foldername,
                              years = queries$years)

    if (geometry_type == "polygon") {
      print("cropping dataset to area coverage..")
      cover_stations <-
        crop_dataset(
          lat_crop = ncdownload$lat_crop,
          lon_crop = ncdownload$lon_crop,
          area = bounding_coords$area_shp,
          area_buff = bounding_coords$area_buff,
          preview = preview
        )
    }


    print("writing station data to csv..")

    if(geometry_type == "polygon"){
      write_stations(
        vardl = ncdownload$vardl,
        cover_stations = cover_stations,
        mn_variables = mn_variables,
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
        area = bounding_coords$area_shp
      )
    }else{
      matrix <- merged_data$mastermatrix
      point_df <- lapply(matrix, as.vector) %>% as.data.frame()
      date = seq(as.POSIXct(fromdate), as.POSIXct(todate), by = "hour") %>% strftime()
      final_df <- cbind(date, point_df) %>% dplyr::as_tibble()
      readr::write_csv(x = final_df, file = paste0(directory, "/",foldername, "/metnoreanalysis3_point.csv"))
      delfile = list.files(paste0(directory,"/",foldername, "/"), pattern = ".rds", full.names = T) %>% file.remove()

      ### metdata file
      meta_df = c(
        lat = paste("lat = ", ncdownload$lat_crop),
        lon = paste("lon = ", ncdownload$lon_crop),
        x = paste("X =", ncdownload$x_crop),
        y = paste("Y =", ncdownload$y_crop),
        elevation = paste("ELEVATION = ", ncdownload$alt_crop)
      )

      source = paste0(
        "Data sourced from MetNordic Reanalysis v3 Dataset Meteorologisk institutt, downloaded by miljotools version ",
        packageVersion("miljotools"),
        " on ",
        Sys.time()
      )

      fileConn <- file(paste0(directory, "/", foldername, "/metadata.txt"))
      writeLines(
        c(
          meta_df,
          source,
          "https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset",
          "https://moritzshore.github.io/miljotools"
        ),
        fileConn
      )
      close(fileConn)
    }

    return(paste0(directory, "/",foldername))
  }


### Downscale resolution
#' Downscale MetNo Reanalysis3 data to daily resolution
#'
#' This function takes the hourly data from get_metno_reanalysis3() and
#' recalculates it into daily (temporal) resolution.
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
#' @importFrom dplyr %>% group_by all_of across summarise last rename
#' @importFrom purrr map
#' @importFrom readr read_csv write_csv
#' @importFrom lubridate date
#' @importFrom stringr str_split
reanalysis3_daily <- function(path, outpath = NULL, verbose = FALSE, precision = 2){

  # remove trailing "/"
  path <- paste0(dirname(path), "/", basename(path))
  if(verbose){cat(green(italic("reading files..\n")))}

  # get the file paths and differentiate between metadata and data
  files <- list.files(path, full.names = T)
  files_short <- list.files(path, full.names = F)
  stations_short <- files_short[which(grepl(x = files,pattern =  "metadata.csv") == FALSE)]

  stations <- files[which(grepl(x = files,pattern =  "metadata.csv") == FALSE)]

  metadata <- files[which(grepl(x = files,pattern =  "metadata.csv"))]

  # metadata.txt is generated if the download was only one "point" location.
  # otherwise it would be metadata.csv. This is how you can tell them apart.
  if("metadata.txt" %in% stations_short){
   stations <- stations[which(grepl(x = stations,pattern =  "metadata") == FALSE)]
   stations_short <- stations_short[which(grepl(x = stations_short,pattern =  "metadata") == FALSE)]

   download_type = "point"
  }

  # Custom function to convert the hourly data to daily data
  # certain considerations need to be made when summing or averaging.
  hourly2daily <- function(data){
    # These will potentially need to be expanded
    sum_these <- c("precipitation_amount", "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time")
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
      dplyr::rename(min_temp = air_temperature_2m)

    # -1 to get rid of the date column and round by precision
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

# supporting functions ----
nc_open_retry <- function(link) {

  nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                      error = function(cond){
                        warning("failed..")
                        return(NA)
                      })


  if (nc_file %>% length() > 1) {
    return(nc_file)
  } else{
    mt_print(TRUE, "nc_open_retry", "retrying donwload with out longwave radiation")

    # https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters
    # find the location in the link where longwave radiation is, and remove it,
    # then try to open the file wihtout this variable.
    split = link %>% stringr::str_split(",", simplify = T)
    longwave_index <- grepl(x = split, pattern =  "longwave") %>% which()
    new_link = paste(split[-longwave_index], collapse = ",")
    new_nc_file <- tryCatch(
      expr = {ncdf4::nc_open(new_link)},
      error = function(cond) {
        warning("failed..", cond, "retry!")
        return(NA)})
    }

  if (length(new_nc_file)>1) {
    mt_print(TRUE, "nc_open_retry", "download sans longradiation succeeded!")
    warning("file missing longwave radiation")
    return(new_nc_file)
  } else{
    mt_print(TRUE, "nc_open_retry", "retrying donwload..")

    attempt = 1
    while ((attempt < 10) & (length(nc_file) == 1)) {
      Sys.sleep(5)
      attempt = attempt + 1
      nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
        error = function(cond) {
          warning("failed..", cond, "retry!")
          return(NA)})
    }

    if (length(nc_file) > 1) {
      mt_print(TRUE, "nc_open_retry", "connection re-established!")
      return(nc_file)
    } else{
      stop("download failed after 10 attempts.")
    }
  }
}
