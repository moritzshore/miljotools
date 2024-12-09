#' Get MetNordic Coordinate Window
#'
#' This function retrieves the coordinate window for downloading MetNordic files
#'
#' @param area_path path to shapefile of extent / point (this must have point or polygon geometry!)
#' @param area_buffer buffer in m to place around extent / point
#' @param preview Logical: preview the coord window?
#'
#' @returns returns a list of the min and max x and y cells for downloading.
#' @export
#'
#' @examples
#' # TODO
metnordic_coordwindow <- function(area_path, area_buffer, preview){

  # get a base file to find the right x y
  mt_print(preview, function_name = "metnordic_coordwindow","getting base file..")
  filename = "https://thredds.met.no/thredds/dodsC/metpparchivev3/2023/01/31/met_analysis_1_0km_nordic_20230131T23Z.nc"
  ncin <- nc_open_stable(filename)
  if(ncin$filename == filename){
    mt_print(preview, function_name = "metnordic_coordwindow","basefile downloaded.")
  }else{stop("error downloading basefile:\n", filename)}
  x <- ncdf4::ncvar_get(ncin, "x")
  y <- ncdf4::ncvar_get(ncin, "y")
  ncdf4::nc_close(ncin)

  # lambert conform conical (the projection used by met reanalysis)
  projection <- "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6371000"
  proj_crs <- sf::st_crs(projection)
  mt_print(preview, function_name = "metnordic_coordwindow","Loading and projecting shapefile...")
  area <- sf::read_sf(area_path)
  # Transform the shapefile to the metno projection
  area <- sf::st_transform(area, crs = proj_crs)

  # get the geometry type (either point or polygon)
  area_attr <- sf::st_geometry(area) %>% attr("class") %>% nth(1)
  mt_print(preview, function_name = "metnordic_coordwindow","geometry detected:", area_attr)

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

    if(preview){
      df <- sf::st_as_sf(x = data.frame(x = x[index_x], y = y[index_y]),
                         coords = c("x", "y"),
                         crs = sf::st_crs(proj_crs))
      area$name = "provided file"
      plot = mapview::mapview(df, layer.name = "Nearest Re-analysis Gridpoint", col.regions = "orange")+
        mapview::mapview(area, layer.name = "User location", col.regions = "blue")

      print(plot)
      mydist <- sf::st_distance(df, area) %>% round(0)
      mt_print(preview, function_name = "metnordic_coordwindow","Note: Selected grid cell distance from provided point is: ", paste0(mydist, " meters"))
    }


    return(list(index_x = index_x, index_y = index_y))

  } else{
    # routine for if the polygon was passed.
    # this warns you if you might not have the right geometry
    if (area_attr != "sfc_POLYGON"){warning("Shapefile type not 'sfc_POLYGON', problems may occur..")}
    # drop the Z coordinate (extra stability)
    area <- sf::st_zm(area)
    # Buffer the shapefile to the user defined amount
    mt_print(preview, function_name = "metnordic_coordwindow","buffering shapefile: ", paste(area_buffer, "m"))

    if(area_buffer > 0){
      area_buff <- sf::st_buffer(x = area, dist = area_buffer, endCapStyle = "FLAT", joinStyle = "BEVEL")
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

    # previewing coverage
    if(preview){
      # old mapview solution
      # plot <- mapview::mapview(wsbox, col.region = "blue")+
      #   mapview::mapview(area_buff, col.region = "red")+
      #   mapview::mapview(area, col.region = "orange")
      # print(plot)
      mt_print(preview, function_name = "metnordic_coordwindow","getting basemap...")
      europe <- suppressMessages(eurostat::get_eurostat_geospatial(resolution = "01",
                                                  nuts_level = "3",
                                                  year = 2024, crs = "3857"))
      europe_lcc <- sf::st_transform(europe, crs = proj_crs)
      mt_print(preview, function_name = "metnordic_coordwindow","finished downloading basemap.")



      outer <- (abs(abs(wsbox[['xmin']]) - abs(wsbox[['ymax']]))) * .1 #
      mymapextent <- sf::st_bbox(sf::st_buffer(area_buff,outer ))

      xmin_out <- mymapextent[['xmin']]
      xmax_out <- mymapextent[['xmax']]
      ymin_out <- mymapextent[['ymin']]
      ymax_out <- mymapextent[['ymax']]


      centroid <- suppressWarnings(sf::st_centroid(area))
      centroid <-  sf::st_transform(centroid, crs = "EPSG:4326")
      center <- st_coordinates(centroid)

      plot_title <- paste("Shapefile middlepoint: \nLON =", center[1], "\nLAT = ", center[2])
      mt_print(preview, function_name = "metnordic_coordwindow","plotting...")

      mymap <- ggplot() +
        geom_sf(data = europe_lcc) +
        geom_sf(data = area_buff, mapping = aes(fill = "Buffer")) +
        geom_sf(data = area, mapping = aes(fill = "Area")) +
        coord_sf(xlim = c(xmin_out, xmax_out), ylim = c(ymin_out, ymax_out))+
        theme(legend.title = element_blank(), legend.position = "bottom")+
        ggtitle(plot_title, paste0("Buffer size = ", area_buffer, " m"))

      print(mymap)
      mt_print(preview, function_name = "metnordic_coordwindow","finished plotting.")

    }
    mt_print(preview, function_name = "metnordic_coordwindow","calculating coordinate window...")

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

    if(preview){mt_print(preview, function_name = "metnordic_coordwindow",
                         "coordinate window is:", paste0("xmin=", index_xmin,
                                                         " xmax=", index_xmax,
                                                         " xmin=",index_ymin,
                                                         " ymax=",index_ymax))}
    return(list(index_xmin = index_xmin,
                index_xmax = index_xmax,
                index_ymin = index_ymin,
                index_ymax = index_ymax))
  }
}

