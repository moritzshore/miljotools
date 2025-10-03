#' Extract data on a (ir)regular grid basis
#'
#' This function takes the files from `metnordic_merge_hourly()` and extracts
#' timeseries at desired locations. If a polygon shapefile is passed to the
#' function, data will be extracted at each grid cells within the (buffered)
#' area. If a point geometry is supplied, data will be extracted at each point. A
#' file is written to disk with .csv file for each grid cell and a metadata file for each grid cell.
#'
#'
#' TODO: add resolution to polygon grid..
#'
#' @param merged_path String, Path to merged ncdf files from `metnordic_merge_hourly()`
#' @param area Geo-referenced shapefile either of polygon or point geometry. Passing a polygon geometry will lead to a regular grid being extracted from all overlaying points.
#' @param buffer Numeric, buffer in meters. Useful for getting grid cells just outside of catchment. Not used if a point geometry is passed.
#' @param mn_variables Character Vector, Met Nordic variables to extract
#' @param outdir String, Folder where data will be written
#' @param verbose Boolean, print status
#'
#' @returns path to written files
#' @export
#'
#' @importFrom sf st_geometry_type
#' @importFrom raster xyFromCell
#'
metnordic_extract_grid <- function(merged_path,
                                   area,
                                   buffer = 0,
                                   mn_variables,
                                   outdir,
                                   verbose) {

  # sub functions
  get_overlapping_cells <- function(merged_path, area_overlap){
    filepaths <- list.files(merged_path, pattern = "metno-", full.names = T)
    rasterfile <- raster::raster(filepaths[1])
    testpoints <- raster::xyFromCell(rasterfile[[1]], cell = 1:length(rasterfile)) %>% as.data.frame() %>% st_as_sf(coords = c("x", "y"),
                                                              crs =  crs(rasterfile))

    grid.sf.proj <- st_transform(testpoints, st_crs(rasterfile))
    area_overlap <- st_transform(area_overlap, st_crs(rasterfile))
    # figure out which ones are touching the area_overlap buffer
    pnts_trans <- grid.sf.proj %>% mutate(
      intersection = as.integer(st_intersects(grid.sf.proj, area_overlap)))
    grid <- grid.sf.proj[which(pnts_trans$intersection == 1),]
    if(verbose){
      required_packages <- c("ggplot2", "tidyterra")
      install_missing_packs(required_packages)
      myrast <- terra::rast(filepaths[1])
      ggplot2::ggplot() +  tidyterra::geom_spatraster(data=myrast[[1]])+
        ggplot2::geom_sf(data = area_buffered, alpha = .3, color = "green")+
        ggplot2::geom_sf(data = area, alpha = .3, color = "lightgreen")+
        ggplot2::geom_sf(data = testpoints, color = "darkorange")+
        ggplot2::geom_sf(data = grid, color = "darkgreen")+
        ggplot2::theme_bw() +  viridis::scale_fill_viridis(option="E")
    }
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
    if(length(filepath) == 0){
      stop("Variable of type `", variable, "` not found! perhaps you have not merged it or downloaded it yet?")
    }
    # load" the raster data
    mt_print(verbose, "extract_grid_cells", text = "extracting:", text2 = variable)
    varrast <- terra::rast(filepath)
    datamatrix <- terra::extract(varrast, grid, method = "bilinear", raw = TRUE, ID = FALSE)
    if(FALSE){
      required_packages <- c("ggplot2", "tidyterra")
      install_missing_packs(required_packages)
      ggplot() +  geom_spatraster(data=varrast[[1]])+
        geom_sf(data = area)+
        geom_sf(data = grid)+
        theme_bw() +  viridis::scale_fill_viridis(option="D")
    }
    datamatrix %>% return()
  }


  # main
  dir.create(outdir)
  writeLines("hi", paste0(outdir, "/hi.txt"))
  # check if regular grid was provided:

   if(st_geometry_type(area) == "POLYGON"){
     regular = TRUE
   }else if((area %>% st_geometry_type() == "POINT") %>% all()){0
     regular = FALSE}else{
       stop("Geometry type not understood! Must be either a single POLYGON or multiple POINT geometries. \n >> You supplied: ", st_geometry_type(area))
     }

  if(regular){
    area_buffered <- sf::st_buffer(area, buffer)
    grid <- get_overlapping_cells(merged_path = merged_path, area_overlap = area_buffered)
  }else{
    # if the grid is not regular, just use the provided shapefile
    grid <- area
  }
  station_nr <- grid$geometry %>% length()
  matrix_list <- lapply(X = mn_variables,FUN =  extract_grid_cells)

  if(regular){
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
      mt_print(verbose, "metnordic_extract_grid", "Working on station", paste(i, "/",station_nr), rflag = T)
      if(verbose){cat("\n")}
      varlist <- lapply(X = matrix_list, get_timeseries)
      yes <- varlist %>% purrr::reduce(full_join, by = "date")
      fp <- paste0(outdir, "/metnordic_extract_grid_", i, ".csv")
      write_csv(x = yes, file = fp)
      metadf = get_meta(directory = merged_path,
                        mn_variables = mn_variables,
                        point = grid[i,],
                        name = paste0("plot", i),
                        verbose = F ## TODO: something is wrong here, the distance should always be 0? some issue with projection?
      )


      metafp <- paste0(outdir, "/METNORDIC_meta_plot", i, ".csv")
      paste(names(metadf), "=", metadf, collapse = "\n") %>% writeLines(con = metafp)
    }
    mt_print(verbose, "extract_grid_cells", text = "Finished")
  }else{
    warning("functionality for non-regular not proofed yet..")
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
      mt_print(verbose, function_name = "metnordic_extract_grid", text = "Working on station", text2 =  paste(i, "/",station_nr), rflag = T)
      if(verbose){cat("\n")}
      varlist <- lapply(X = matrix_list, get_timeseries)
      yes <- varlist %>% purrr::reduce(full_join, by = "date")
      fp <- paste0(outdir, "/METNORDIC_point_plot", i, ".csv")
      write_csv(x = yes, file = fp)

      # TODO: rewrite this, its super slow.
      metadf = get_meta(directory = merged_path,
                        mn_variables = mn_variables,
                        point = area[i,],
                        name = paste0("plot", i),
                        verbose = verbose
      )
      metafp <- paste0(outdir, "/METNORDIC_meta_plot", i, ".csv")
      paste(names(metadf), "=", metadf, collapse = "\n") %>% writeLines(con = metafp)
    }
  }
  mt_print(verbose, function_name = "metnordic_extract_grid", text = "Finished.")
  return(outdir)
}
