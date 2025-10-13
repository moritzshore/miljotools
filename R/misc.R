#' custom print function for miljotools
#'
#' @param verbose print or not
#' @param function_name function name string
#' @param text  string 1
#' @param text2  string 2
#' @param rflag in place replacement flag
#'
#' @return nothing
#' @keywords internal
#'
#' @importFrom crayon bold bgGreen italic bgBlue bgYellow black bgCyan yellow underline bgWhite cyan white
#'
#'
#'
mt_print <- function(verbose, function_name, text, text2 = NULL, rflag = FALSE) {
  miljotheme <- bold$bgGreen
  tools_theme <- bold$italic$bgBlue

  if (grepl("cwatm", x = function_name)) {
    f_theme  <- bgYellow$black$bold
  }else if(grepl("thermopluviogram", x = function_name)){
    f_theme = bgWhite$cyan$bold
    # TODO: add GREPing senorge / metnordic for themeing
  }else if(grepl("senorge", x = function_name)){
    f_theme = bgYellow$white$bold
  }else if(grepl("metnordic", x = function_name)){
    f_theme <- bgCyan$white$bold

  }else{
    f_theme <- bgBlack$white$bold

  }
  text_theme <- italic$yellow
  text_2_theme <- black $ underline

  if(rflag){
    prefix = "\r"
    suffix = NULL
  }else{
    prefix = NULL
    suffix = "\n"
  }
  if (verbose) {
    cat(
      prefix,
      miljotheme("miljo"),
      bgBlue("\U1F33F"),
      tools_theme("tools "),
      f_theme(paste0("", function_name, "")),
      text_theme(" >>", text, ""),
      text_2_theme(text2),
      suffix,
      sep = ""
    )
  }
}

install_missing_packs <- function(required_packages) {
  missing_packs <- which((required_packages %in% utils::installed.packages()) == FALSE)
  if(length(missing_packs) > 0){
    mt_print(TRUE, "", "Missing CRAN packages are required to run this function, installing now:",
            paste0(required_packages[missing_packs], sep = " "))
    utils::install.packages(required_packages[missing_packs])
  }
  if(length(which((required_packages %in% utils::installed.packages()) == FALSE)) > 0){
    stop("following packages failed to install:\n",
         required_packages[which((required_packages %in% utils::installed.packages()) == FALSE)])
  }
}

get_overlapping_cells <- function(directory, variables, area, buffer, verbose){
  area_buffered <- area %>% sf::st_buffer(buffer)
  filepaths <- list.files(directory, full.names = T)
  if(length(filepaths) == 0){
    stop("No files found! (Make sure to provide a path to a directory, not a file)\nIn: >>",directory, "<<")
  }
  rasterfile <- raster::raster(filepaths[1], varname = variables[1])
  testpoints <- raster::xyFromCell(rasterfile[[1]], cell = 1:length(rasterfile)) %>%
    as.data.frame() %>%
    sf::st_as_sf(coords = c("x", "y"), crs =  terra::crs(rasterfile))
  grid.sf.proj <- sf::st_transform(testpoints, sf::st_crs(rasterfile))
  area_buffered <- sf::st_transform(area_buffered, sf::st_crs(rasterfile))
  # figure out which ones are touching the area_buffered buffer
  pnts_trans <- grid.sf.proj %>% dplyr::mutate(
    intersection = as.integer(sf::st_intersects(grid.sf.proj, area_buffered)))
  grid <- grid.sf.proj[which(pnts_trans$intersection == 1),]
  if(verbose){
    required_packages <- c("ggplot2", "tidyterra")
    install_missing_packs(required_packages)
    # quietly cuz of annoying warning messages that i cant turn off
    terra::rast(filepaths[1], variables[1]) -> myrast
    ggplot2::ggplot() +  tidyterra::geom_spatraster(data=myrast[[1]], show.legend = F)+
      ggplot2::geom_sf(data = area_buffered, alpha = .3, color = "green")+
      ggplot2::geom_sf(data = area, alpha = .3, color = "lightgreen")+
      ggplot2::geom_sf(data = testpoints, color = "darkorange")+
      ggplot2::geom_sf(data = grid, color = "darkgreen")+
      ggplot2::theme_bw() +  viridis::scale_fill_viridis(option="E")+
      ggplot2::ggtitle("Overlapping grid cells")-> plot
    print(plot)
  }
  return(grid)
}
