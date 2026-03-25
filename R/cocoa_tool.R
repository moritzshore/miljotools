# rmd check appeastment
type <- from_id <- to_id <- obj_typ <- hyd_typ <- frac <- direction <- percent <- id <- name<- cha_id<- flow<- gen_type<- label<- gis_id<-NULL

# cocoa_vis(
#   buildR_dir = "../SWAT-skuterud/project_data/SWATbuildR/skuterud_buildR/",
#   directory = "../SWAT-skuterud/project_data/figures/connectivity",
#   RERENDER = T,
#   verbose = T
# )
# #

#' Visualize COCOA on a catchment scale
#'
#' NOTE: this function has a few requirements to work, please see below.
#'
#' 1. This function only works with SWATbuildR output, along side with a generated `land_connections_as_lines.shp` file which can be generated using [this code](https://github.com/MR-Eini/Mini_setup_CREATE/blob/main/1_Setup/Libraries/create_connectivity_line_shape.R).
#' 2. The land use theme is set up for my specific catchment. If you need to add to this theme then please send a pull request with the additional legend entries.
#'
#' @seealso [cocoa_hru()]
#'
#' @param buildR_dir (string) path to SWATbuildR project directory (containing the 'data/*' subdirectory)
#' @param directory (string) directory where to save/load the generated HRU plots to/from. (will be saved in a sub-directory 'hru_report/')
#' @param RERENDER (logical) re-generate the HRU plots?
#' @param verbose (logical) print status?
#'
#' @returns `mapview` map of HRUs in the catchment. Click to view each HRUs connectivity.
#'
#' @export
cocoa_vis <- function(buildR_dir,
                      directory,
                      RERENDER = TRUE,
                      verbose = TRUE) {

  # Get required packages
  required_packages <- c("leafpop", "naturalsort")
  install_missing_packs(required_packages)

  # load data needed for mapping
  sf::read_sf(paste0(buildR_dir, "data/vector/hru.shp")) -> hru.shp
  sf::read_sf(paste0(buildR_dir, "data/vector/res.shp")) -> res.shp
  sf::read_sf(paste0(buildR_dir, "data/vector/cha.shp")) -> cha.shp

  # generate HRU reports
  if (RERENDER) {
    unlink(paste0(directory,"/hru_report"), recursive = T)
    lapply(
      X = hru.shp$id,
      FUN = cocoa_hru,
      buildR_dir = buildR_dir,
      directory = directory,
      save_to_png = TRUE,
      verbose = verbose
    ) -> DUMP
  }

  # load filepaths for images
  list.files(paste0(directory, "/hru_report"), full.names = T) %>% naturalsort::naturalsort() -> imgs
  if(length(imgs) == 0){stop("Found ", length(imgs), " HRU reports! did you pass the correct `directory`?)\n >> directory = ", directory)}
  if(length(imgs) != length(hru.shp$id)){warning("Found ", length(imgs), " HRU reports for ", length(hru.shp$id), " --not all HRUs will have reports! (did you pass the correct `directory`?)")}
  # generate maps
  mapview::mapview(cha.shp, layer.name = "Channels", legend = FALSE) -> channelmap
  mapview::mapview(res.shp, layer.name = "Water boides", legend = FALSE) -> watrmap
  popupmap <- mapview::mapview(
    hru.shp,
    legend = FALSE,
    alpha.regions = 0,
    color = "black",
    lwd  = 1,
    popup = leafpop::popupImage(
      imgs,
      src = "local",
      embed = FALSE,
      width = 600
    )
  )
  watrmap + channelmap + popupmap -> full_map
  full_map %>% return()
}

#' Generates a connectivity plot for an HRU
#'
#' There are some important details which need to be observed for using this function, they are listed below.
#'
#' 1. This function only works with SWATbuildR output, along side with a generated `land_connections_as_lines.shp` file which can be generated using [this code](https://github.com/MR-Eini/Mini_setup_CREATE/blob/main/1_Setup/Libraries/create_connectivity_line_shape.R).
#' 2. The land use theme is set up for my specific catchment. If you need to add to this theme then please send a pull request with the additional legend entries.
#'
#' @seealso [cocoa_vis()]
#'
#' @param hru_id (integer) ID of HRU to plot
#' @param buildR_dir (string) path to SWATbuildR project directory (containing the 'data/*' subdirectory)
#' @param agri_pattern (string) a pattern which identifies agriculturial fields (REGEX)
#' @param directory (string) directory where to save plots (optional, only if `save_to_png` is enabled. Will be created to sub-directory 'hru_report/'. If left blank, will use working directory).
#' @param save_to_png (logical) `TRUE` = saves to file, `FALSE` = returns plot
#' @param verbose (logical) Print status?
#'
#' @importFrom ggplot2 ggplot unit alpha ggsave geom_sf_label geom_sf aes ggtitle scale_linetype_manual scale_color_manual scale_fill_manual theme_bw element_blank guide_legend guides coord_sf theme arrow
#' @importFrom sf st_as_sf st_as_sfc st_bbox st_crs read_sf st_cast st_drop_geometry
#' @importFrom terra crop res minmax
#' @returns Either the path to the plot if `save_to_png` is `TRUE`, or returns the ggplot object itself if `save_to_png` is `FALSE`
#' @export
#'
cocoa_hru <- function(hru_id, buildR_dir, agri_pattern = "a_", directory = NULL, save_to_png = FALSE, verbose = TRUE){

  required_packages <- c("isoband", "tidyterra")
  install_missing_packs(required_packages)

  mt_print(verbose, "cocoa_hru", "Working on HRU:", hru_id, rflag = F)
  if (is.null(directory)) {
    directory = getwd()
    mt_print(verbose, "cocoa_hru", "Setting directory to working directory:", directory, rflag = F)
  }
  if (save_to_png) {
    hru_dir = paste0(directory, "/hru_report")
    dir.create(hru_dir, showWarnings = F)
    mt_print(verbose, "cocoa_hru", "Will save file to directory:", hru_dir, rflag = F)
  }
  mt_print(verbose, "cocoa_hru", "Loading SWATbuildR data...", rflag = F)
  sf::read_sf(paste0(buildR_dir, "data/vector/hru.shp")) -> hru.shp
  hru.shp %>% dplyr::mutate(gen_type = dplyr::if_else(grepl(pattern = agri_pattern, x = type), true = "agri", false = type)) -> hru.shp
  sf::read_sf(paste0(buildR_dir, "data/vector/res.shp")) -> res.shp
  sf::read_sf(paste0(buildR_dir, "data/vector/cha.shp")) -> cha.shp
  sf::read_sf(paste0(buildR_dir, "data/vector/basin.shp")) -> basin.shp
  if(file.exists(paste0(buildR_dir, "data/vector/land_connections_as_lines.shp")) == FALSE){
    stop("You need to generate the 'land_connections_as_lines.shp' file first! and it needs to be in: ",
      buildR_dir, "vector/", "\n",
      "You can generate it using the code from here:", "\n",
      "https://github.com/MR-Eini/Mini_setup_CREATE/blob/main/1_Setup/Libraries/create_connectivity_line_shape.R")
  }
  sf::read_sf(paste0(buildR_dir, "data/vector/land_connections_as_lines.shp")) -> lcal
  DEM <- terra::rast(paste0(buildR_dir, "data/raster/dem.tif"))
  DEM %>%  terra::res() %>% round(2) %>% paste(collapse = "x") %>% paste0(., "m") -> demrestring
  mt_print(verbose, "cocoa_hru", "Processing data...", rflag = F)
  lcal %>% dplyr::filter(from_id %in% hru_id | to_id %in% hru_id) %>%
    # we dont want these, because its not relevant.
    dplyr::filter(!((obj_typ == "sdc") & (to_id == hru_id))) %>%
    # sometimes the connections get duplicated, idk why... the ID is not needed for anything anyway.. (right?)
    dplyr::group_by(from_id, to_id, obj_typ, hyd_typ) %>%
    dplyr::summarize(frac = sum(frac) %>% round(2), geometry = sf::st_union(geometry), .groups = "drop_last") %>% dplyr::ungroup() -> local_connections
  # extracting outflow hrus
  local_connections %>% dplyr::filter(obj_typ == "ru") %>%  dplyr::filter(from_id == hru_id) %>% dplyr::pull(to_id) -> outflowhrus
  # extracting inflow hrus
  local_connections %>% dplyr::filter(obj_typ == "ru") %>% dplyr::filter(to_id == hru_id) %>% dplyr::pull(from_id) -> inflowhrus
  # outflow channels
  local_connections %>% dplyr::filter(obj_typ == "sdc") %>%  dplyr::filter(from_id == hru_id) %>% dplyr::pull(to_id) -> outflowcha
  local_connections %>%
    # setting their direction
    dplyr::mutate(direction = dplyr::if_else(from_id %in% inflowhrus,
                               "INFLOW", "OUTFLOW")) %>%
    # create a percentage label
    dplyr::mutate(percent = round(frac, 2) * 100) -> local_connections
  # creating a table of outflow data
  local_connections %>% sf::st_drop_geometry() %>%
    dplyr::filter(direction == "OUTFLOW") %>%
    dplyr::select(id =  to_id, frac,obj_typ, hyd_typ, percent) -> ljo
  # creating a table of inflow data
  local_connections %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(direction == "INFLOW") %>%
    dplyr::select( id = from_id, frac, obj_typ, hyd_typ, percent) -> lji

  local_connections %>% dplyr::filter(obj_typ == "ru") -> local_hrus
  # creating the to-map hru file
  # using relevant hrus to filter
  c(local_hrus$from_id, local_hrus$to_id) %>% unique() -> relhru
  hru.shp %>% dplyr::filter(id %in% relhru) %>%
    # determine direction
    dplyr::mutate(direction = dplyr::if_else(condition = id %in% inflowhrus,
                               true = "INFLOW", false = "OUTFLOW")) %>%
    # set hru of interest direction to NA
    dplyr::mutate(direction = dplyr::if_else(id == hru_id, NA, direction)) %>%

    # join the AT data
    dplyr::left_join(rbind(lji, ljo), by = "id") %>%
    dplyr::mutate(hyd_typ = dplyr::if_else(id == hru_id, "target", hyd_typ)) -> maphru
  maphru %>% dplyr::group_by(id, name, type, cha_id, flow, gen_type, direction, obj_typ) %>%
    dplyr::summarise(label = paste0("[", hyd_typ, " ", percent, "%]", collapse = "\n"),
              geometry = sf::st_union(geometry), .groups = "drop_last") %>%
    dplyr::mutate(label = dplyr::if_else(id == hru_id, paste0("HRU",hru_id), paste0("HRU", id, "\n", label))) -> maphru2
  # extract hru of interest
  maphru2 %>% dplyr::filter(id == hru_id) -> the_HRU
  # define bounding box:
  # TODO: find out how reservoirs rout water?
  local_connections %>% dplyr::filter(obj_typ == "res") %>% dplyr::filter(to_id != hru_id) %>% dplyr::rename(id = to_id) -> to_res # i dont think its possible to route water from resivour to hru?
  rbind(maphru2 %>% dplyr::select(geometry),
        to_res %>% dplyr::select(geometry),
        local_connections %>% dplyr::filter(obj_typ == "sdc") %>%  dplyr::select(geometry)) -> all_objs
  all_objs %>% sf::st_bbox() -> cwbbox
  # adjust bbox
  cwbbox['xmax'] = cwbbox['xmax'] + 50
  cwbbox['ymax'] = cwbbox['ymax'] + 50
  cwbbox['ymin'] = cwbbox['ymin'] - 50
  cwbbox['xmin'] = cwbbox['xmin'] - 50
  ## Coordinate System
  mycs <- ggplot2::coord_sf(
    xlim  = c(cwbbox[["xmin"]], cwbbox[["xmax"]]),
    ylim = c(cwbbox["ymin"], cwbbox["ymax"]),
    crs = sf::st_crs(maphru),
    datum = sf::st_crs(maphru),
    label_graticule = "NW"
  )
  sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(cwbbox)), crs=sf::st_crs(hru.shp)) -> cwbbox_shp


  # Dem parsing
  mydemcrop <- terra::crop(DEM, cwbbox_shp)
  resolution_string <- paste0(mydemcrop %>% terra::res() %>% paste(collapse = " x "), "m")
  alt_limits <- terra::minmax(mydemcrop) %>% as.vector()
  # Round to lower and higher 500 integer with a min of 0
  alt_limits <- pmax(c(floor(alt_limits[1] / 10), ceiling(alt_limits[2] / 10)), 0) * 10

  ## GGPLOT ## ------------

  # themes, scaling, etc.
  {
    mt_print(verbose, "cocoa_hru", "Plotting COCOA..", rflag = F)

    ## Guides
    myguide <- guides(
      fill = guide_legend(
        title = "Landuse",
        position = "right",
        override.aes = list(linetype = NA)
      ),
      color = guide_legend(position = "bottom", title = "Direction of Flow:"),
      linetype = guide_legend(
        position = "left",
        direction = "vertical",
        override.aes = list(color = "black")
      )
    )
    ## Theme
    mytheme <-  theme_bw() +  theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
    )
    ## Scaling
    landuse_theme <- scale_fill_manual(
      values = c(
        "darkgreen",
        "lightgreen",
        "lightblue",
        "gold3",
        "grey",
        "gray55",
        "darkolivegreen3",
        "darkred",
        "blue"
      ),
      breaks = c(
        "frst",
        "past",
        "watr",
        "agri",
        "utrn",
        "urml",
        "rngb",
        "wetf",
        "Constructed Wetland"
      ), labels = c("Forest", "Pasture", "Lakes", "Agriculture", "Roads", "Urban", "Rangeland", "Wetland", "Constructed\nWetland"))

    my_color_scale <- scale_color_manual(breaks = c("OUTFLOW", "INFLOW") %>% rev(),
                                         values = c("orange3", "royalblue3") %>% rev())

    my_linetype_scale <- scale_linetype_manual(
      breaks = c("tot",   "sur",    "lat",    "til",     "target"),
      values = c("dashed", "twodash", "dotted", "dotdash", "solid"),
      na.translate = T)

    mytitle <- ggtitle(paste0("'COCOA' of HRU", hru_id),
                       paste0("DEM Resolution: ", resolution_string))
  }


  ## Baseplot
  ggplot() +
    # contour lines
    tidyterra::geom_spatraster_contour_text(maxcell = 500973,
                                 data = mydemcrop,
                                 color = alpha("grey", 1),
                                 inherit.aes = F,
                                 breaks = seq(
                                   from = alt_limits[1],
                                   to = alt_limits[2],
                                   length = 15
                                 ) %>% round(0) %>% unique(),
                                 label_placer = isoband::label_placer_minmax(
                                   placement = "tblr",
                                   rot_adjuster = isoband::angle_halfcircle_bottom(),
                                   n = 1
                                 )
    ) +
    # hru basemap
    geom_sf(data = hru.shp, aes(fill = gen_type), alpha = .075) +
    # hru's of interest
    geom_sf(data = maphru2, aes(fill = gen_type, color = direction),
            linewidth = 1.1, alpha = .4) +
    # highlight current hry
    geom_sf(data = the_HRU, linetype = "dashed", color = alpha("yellow", 1), linewidth = 1.1, fill = NA) +
    # plot resivoiurs
    geom_sf(data = res.shp, fill = "blue", alpha = .4) +
    # mark basin outline
    geom_sf(data = basin.shp, color = "red", fill = NA) +
    # plot channels
    geom_sf(data = cha.shp, color = "blue",  alpha = .4,
            arrow = arrow(type = "closed", angle = 20, length = unit(3, "lines")))  -> baseplot

  # adding outflow channels if present
  if(length(outflowcha) > 0){
    show_legend_flag = (local_hrus %>% dplyr::filter(direction == "OUTFLOW") %>% pull(from_id) %>% length() == 0)
    local_connections %>% dplyr::filter(obj_typ == "sdc") -> out_cha_shp
    # get channel geometry:
    cha.shp %>% dplyr::filter(id %in% out_cha_shp$to_id) %>% dplyr::select(to_id = id,geometry) -> chageo

    # assign label geometry:
    cha_out <- st_cast(chageo, "POINT", warn = F) %>% last()

    left_join(chageo, st_drop_geometry(out_cha_shp), by = "to_id") -> out_cha_shp

    left_join(cha_out,  st_drop_geometry(out_cha_shp), by = "to_id") -> cha_out

    baseplot + geom_sf(data = out_cha_shp, mapping =  aes(color = direction), linetype = "solid", alpha = .75, size = 2,
                       show.legend =show_legend_flag, inherit.aes = F)  +
      geom_sf_label(data = cha_out, mapping = aes(label = paste0("CHA", to_id, "\n", hyd_typ,  "[", percent, "%]"),
                                                  color = direction), size = 2,
                    alpha = .9, show.legend = F, nudge_y = 17) -> baseplot
  }
  # adding reservoirs if present

  if(length(to_res %>% pull(id)) > 0){
    res.shp %>% dplyr::select(-gis_id) %>% dplyr::filter(id %in% (to_res %>% pull(id))) -> resmap
    left_join(resmap, to_res %>% st_drop_geometry(), by = "id" ) -> resmap
    baseplot + geom_sf(data = resmap, aes(color = direction),linewidth = 1.1, alpha = .3) +
      geom_sf_label(data = resmap, mapping = aes(label = paste0("RES", id, "\n","[",hyd_typ," ", percent, "%]"),
                                                 color = direction), size = 2,
                    alpha = .9, show.legend = F, nudge_y = 30) -> baseplot
  }
  # if tile drained:
  local_connections %>% dplyr::filter(hyd_typ == "til") %>% pull(to_id) -> tile_drained
  if(tile_drained %>% length() > 0){
    maphru2 %>% mutate(label = if_else(id == hru_id, paste0("HRU", id, "\n","(tiledrain->cha", tile_drained, ")"), label)) -> maphru2
  }
  # label hrus of interest
  baseplot + geom_sf_label(data = maphru2,
                           aes(label = label,
                               color = direction), size = 2,
                           alpha = .75, show.legend = F) -> baseplot

  adjusted_plot <- baseplot + mycs + myguide + mytheme + my_color_scale + my_linetype_scale + landuse_theme + mytitle
  if(save_to_png){
    mt_print(verbose, "cocoa_hru", "Saving plot..", rflag = F)

    plotpath <-  paste0(hru_dir,"/HRU_", hru_id, ".png")
    ggsave(plot = adjusted_plot, filename = plotpath, create.dir = T, dpi = 300,
           units = "cm", limitsize = T, height = 20, width = 20, bg = "white", scale = .75)
    mt_print(verbose, "cocoa_hru", "Plot saved:", text2 =plotpath, rflag = F)
    return(plotpath)
  }else{
    mt_print(verbose, "cocoa_hru", "Returning plot.", rflag = F)
    return(adjusted_plot)
  }
}

