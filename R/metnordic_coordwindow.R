#' Get a MET Nordic Coordinate Window
#'
#' This function retrieves the coordinate window from a shapefile for
#' downloading MET Nordic files. This window can then be passed to
#' `metnordic_buildquery()` to parse OPENDAP urls to download.
#'
#' @seealso [metnordic_buildquery()]
#'
#' @author Moritz Shore
#'
#' @param area_path String: path to shapefile of region / point (this must have point or polygon geometry!)
#' @param area_buffer Integer: buffer in m to place around shapefile / point
#' @param verbose Logical: plot the coordinate window?
#'
#' @returns returns a list of the min and max x and y cells for downloading.
#' @export
#'
#' @examples
#' # TODO
#' @importFrom sf 'st_crs<-' read_sf st_bbox st_buffer st_zm
#' @importFrom dplyr rename
#' @importFrom mapview mapview
metnordic_coordwindow <- function(area_path, area_buffer = 0, verbose = FALSE){

  if(area_buffer < 0){
    area_buffer = 0
  }

  # if it already is a shape file, then no need to read it
  if(methods::is(area_path, "sf")){
    area <- area_path
  }else if(methods::is(area_path, "character")){
    # if it is a string, then read it
    area <- sf::read_sf(area_path)
  }else{
    stop("`area` parameter not recognized! please pass either a filepath to a .shp file, or an `sf` object!\n")
  }

  # get a base file to find the right x y
  mt_print(verbose, function_name = "metnordic_coordwindow","getting base file..")
  filename = "https://thredds.met.no/thredds/dodsC/metpparchivev3/2023/01/31/met_analysis_1_0km_nordic_20230131T23Z.nc"
  ncin <- nc_open_retry(filename)
  if(ncin$filename == filename){
    mt_print(verbose, function_name = "metnordic_coordwindow","basefile downloaded.")
  }else{stop("error downloading basefile:\n", filename)}
  x <- ncdf4::ncvar_get(ncin, "x")
  y <- ncdf4::ncvar_get(ncin, "y")
  ncdf4::nc_close(ncin)

  # lambert conform conical (the projection used by met reanalysis)
  projection <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6371000"
  proj_crs <- sf::st_crs(projection)
  mt_print(verbose, function_name = "metnordic_coordwindow","Loading and projecting shapefile...")
  # Transform the shapefile to the metno projection
  area <- sf::st_transform(area, crs = proj_crs)

  # get the geometry type (either point or polygon)
  area_attr <- sf::st_geometry(area) %>% attr("class") %>% dplyr::nth(1)
  mt_print(verbose, function_name = "metnordic_coordwindow","geometry detected:", area_attr)

  # routine for if a point was passed
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

    df <- sf::st_as_sf(x = data.frame(x = x[index_x], y = y[index_y]),
                       coords = c("x", "y"),
                       crs = sf::st_crs(proj_crs))
    metadist <- (sf::st_distance(df, area) %>% round(0))[1,1]

    if(verbose){

      area$name = "provided file"
      plot = mapview::mapview(df, layer.name = "Nearest Re-analysis Gridpoint", col.regions = "orange")+
        mapview::mapview(area, layer.name = "User location", col.regions = "blue")
      print(plot)
      mt_print(verbose, "metnordic_coordwindow",
               "Note: Selected grid cell distance (in meters) to desired location:",metadist)
    }

    return(list(index_x = index_x, index_y = index_y, metadist = metadist))

  } else{
    # routine for if the polygon was passed.
    # this warns you if you might not have the right geometry
    if (area_attr != "sfc_POLYGON"){warning("Shapefile type not 'sfc_POLYGON', problems may occur..")}
    # drop the Z coordinate (extra stability)
    area <- sf::st_zm(area)
    # Buffer the shapefile to the user defined amount
    mt_print(verbose, function_name = "metnordic_coordwindow","buffering shapefile: ", paste(area_buffer, "m"))

    if(area_buffer > 0){
      area_buff <- sf::st_buffer(x = area, dist = area_buffer)
    } else{
      area_buff <- area
    }
    # get the bounding box of this shape
    wsbox <- sf::st_bbox(area_buff)
    # Grabbing the corners
    ymin <- wsbox[["ymin"]]
    ymax <- wsbox[["ymax"]]
    xmin <- wsbox[["xmin"]]
    xmax <- wsbox[["xmax"]]

    mt_print(verbose, function_name = "metnordic_coordwindow","calculating coordinate window...")

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
    metadist = NA

    if(verbose){
      mt_print(verbose, function_name = "metnordic_coordwindow",
                         "coordinate window is:", paste0("xmin=", index_xmin,
                                                         " xmax=", index_xmax,
                                                         " xmin=",index_ymin,
                                                         " ymax=",index_ymax))
      ## previewing coverage
      # Define the bounding box
      bbox_coords <- c(x[index_xmin], y[index_ymin], x[index_xmax], y[index_ymax])
      names(bbox_coords) = c("xmin","ymin","xmax","ymax")
      bbp = sf::st_as_sfc(sf::st_bbox(bbox_coords))
      sf::st_crs(bbp) = sf::st_crs(area)
      # Define Grid cells
      Var1 <- Var2 <- NULL # RMD CHECK APEASEMENT
      xs <- which(x >= x[index_xmin] & x <= x[index_xmax])
      ys <- which(y >= y[index_ymin] & y <= y[index_ymax])
      expand.grid(x[xs],y[ys]) %>% tibble::as_tibble() %>%
        dplyr::rename(x = Var1, y = Var2) %>%
        sf::st_as_sf(coords = c("x", "y"), crs = proj_crs) -> gridpoints
      # Map view
          mapview::mapview(wsbox, col.region = "grey",alpha.region = .3, layer.name = "Buffer bounding box", legend = FALSE, label = "Buffer BBOX")+
          mapview::mapview(area_buff, col.region = "lightblue",alpha.region = .3, layer.name = "Buffer", legend = F)+
          mapview::mapview(area, col.region = "white", alpha.region = .3,layer.name = "Provided Polygon", legend = FALSE)+
          mapview::mapview(bbp, col.region = "blue", alpha.region = .3, layer.name = "MET Nordic Subset", legend = F)+
          mapview::mapview(gridpoints, legend = FALSE) -> mymap
        print(mymap)
    }

    return(list(index_xmin = index_xmin,
                index_xmax = index_xmax,
                index_ymin = index_ymin,
                index_ymax = index_ymax,
                metadist = metadist))
  }
}

