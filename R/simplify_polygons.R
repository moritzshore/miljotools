geometry <- flag <- idx <- length_intersection <- MILJOTOOLS_TYPE <- . <- NULL

#' Simplify Polygons
#'
#' This function merges flagged polygons into neighboring polygons with the
#' longest shared border. It will prioritize neighboring polygons of the same
#' type. See parameter descriptions for more details.
#'
#' @param polygon_map shapefile containing a column `flag` of type `string` with labels for every polygon as either 'flagged' or 'ok'. Flagged polygons will be simplified.
#' @param type character string of the name of the `type` column in your shapefile. Polygons of the same type will be prioritized for merging If you do not need this feature, simply set the type column to your ID column.
#' @param interactive `TRUE/FALSE` Setting this parameter to true will let you inspect the changes made.
#' @param verbose print actions to console?
#'
#' @returns Returns a modified version of `polygon_map` with the simplifications.
#' @export
#'
#' @importFrom sf st_drop_geometry st_union st_touches st_agr st_cast st_intersection st_length
#' @importFrom dplyr filter pull mutate first arrange desc
#' @importFrom units as_units
#'
#'
#' @examples
#'
#' # See Vignette
simplify_polygons <- function(polygon_map,
                              type,
                              interactive = FALSE,
                              verbose = TRUE) {
  if (interactive) {
    verbose = TRUE
  }
  fname = "simplify_polygons"
  if((type %in% colnames(polygon_map)) == FALSE){
    stop("column '", type, "' does not exist in 'polygon_map'")
  }
  polygon_map$MILJOTOOLS_TYPE <- polygon_map[[type]] %>% sf::st_drop_geometry()
  to_simp <- polygon_map
  to_simp$idx <- seq_along(to_simp$flag)
  to_simp %>% dplyr::filter(flag == "flagged") %>% dplyr::pull(idx) -> flagged_issues

  if(length(flagged_issues) == 0){
    mt_print(verbose,fname, "no flagged issues found, returning dataset")
    return(polygon_map)
  }else{
    mt_print(verbose, fname, "Issues detected, attempting to solve.", paste0("(",flagged_issues %>% length(), ")"))
  }

  # this needs to be an iterative for-loop because the resolution depends on
  # previous actions taken
  i = 0
  imax =  flagged_issues %>% length()
  for (polygon in flagged_issues) {
    i = i + 1
    mt_print(verbose, fname, "working on polygon:", paste0("idx ",polygon, " [", i, "/", imax, "]"))
    # the problem polygon
    problem_poly <- to_simp %>% dplyr::filter(idx == polygon)

    # checking polygon validity.
    if(length(problem_poly$geometry) > 1){
      stop("idx", polygon, " >> polygon has multiple geometries, can't handle this!")
      problem_poly <-  problem_poly %>% sf::st_union()
    }
    if(problem_poly$geometry %>% length() == 0){
      stop("idx", polygon, " >> polygon has NO geometry, can't handle this!")
    }

    # the polygon IDs that touch it
    touchers <-  sf::st_touches(problem_poly, to_simp, sparse = T) %>% unlist()
    neigh <- to_simp[touchers,]
    touching_idx <- neigh$idx
    mt_print(verbose, fname, "the following polygons contact the problem poly:", paste0(touching_idx, collapse = ", "))

    # the polygons that touch it
    if(length(touching_idx) == 0){
      print(paste0("> polygon has no neighbors! removing..."))
      to_simp <- to_simp %>% dplyr::filter(idx != polygon)
      next()
    }

    # mapping it
    if(interactive){
      non_neighbors <- to_simp  %>% filter(idx %in% c(touching_idx, polygon) == FALSE)
      neighbors <- to_simp %>% filter(idx %in% touching_idx)
      mapview::mapview(non_neighbors, col.region = "grey", label = "idx") +
      mapview::mapview(neighbors, col.region = "purple", label = "idx") +
      mapview::mapview(problem_poly, col.region = "orange",  label = "idx") -> map
      print(map)
      readline(prompt = "Click on 'problem_poly' at the bottom right of the map to zoom in. Press [enter] to continue")
    }

    # potential polygons to join to
    candidates = to_simp %>% dplyr::filter(idx %in% touching_idx)

    if(problem_poly$MILJOTOOLS_TYPE %in% candidates$MILJOTOOLS_TYPE){
      # if there is a candidate of the same type involved, we should try to place it in there.
      mt_print(verbose, fname, "neighbor of same type detected..")
      neighborg_same_type <- candidates %>% dplyr::filter(MILJOTOOLS_TYPE == problem_poly$MILJOTOOLS_TYPE)
      # if there is more than one candidate of the same type, pick the one with the largest shared border.
      if (length(neighborg_same_type$MILJOTOOLS_TYPE) > 1) {
        mt_print(verbose, fname, "more than one neighbor of same type exists, will merge with the one which has the longest shared border")
        merge_with_idx <- longest_shared_perimeter(problem_poly = problem_poly, neigh = neighborg_same_type)
        merge_candiate <- neighborg_same_type %>% dplyr::filter(idx == merge_with_idx)
      }else{
        # otherwise just assign the single polygon
        merge_candiate <- neighborg_same_type
      }
    }else{
      mt_print(verbose, fname, paste0("the polygon is of type `", problem_poly$MILJOTOOLS_TYPE, "`. no matching type is amoung the neighbors"))
      # if there is not a match of the same type and there is just one candidate, join the other polygon:
      if(length(candidates$geometry) == 1){
        mt_print(verbose, fname, "There is only one other neighboring polygon, will merge with this one:",  candidates$idx)
        merge_with_idx <- candidates$idx
      }else{
        # otherwise  by longest perimeter
        merge_with_idx <- longest_shared_perimeter(problem_poly = problem_poly, neigh = candidates)
        mt_print(verbose, fname,
                 "There are multiple neighbors, will merge with the candiate with the longest shated border",
                 paste0("(idx ", merge_with_idx, ")"))

      }
      merge_candiate <- candidates %>% filter(idx == merge_with_idx)
    }

    mt_print(verbose, fname, "Checks complete.. Merging:",
             paste("polygon", problem_poly$idx, "into polygon", merge_candiate$idx))
    # to quiet warnings
    sf::st_agr(merge_candiate) = "constant"
    sf::st_agr(problem_poly) = "constant"
    merged_with_nei <- sf::st_union(merge_candiate, problem_poly %>% select(geometry))
    # remove the problem and candidate from the big map
    to_sim_sans_prob_and_nei <- to_simp %>% dplyr::filter(idx %in% c(merge_candiate$idx, problem_poly$idx) == FALSE)

    # join to full map
    to_simp_tenative <- rbind(merged_with_nei, to_sim_sans_prob_and_nei)

    if(interactive){
      new_polygon <- merged_with_nei
      other_polygons <- to_simp_tenative
      newpolymap <- mapview::mapview(new_polygon, col.region = "green",  label = "idx")
      probpolymap <- mapview::mapview(problem_poly, col.region = 'red', alpha.region = .3,  label = "idx")
      newlanduse <- mapview::mapview(other_polygons, col.region = "grey", alpha.region = .1,  label = "idx")
      bigmap <- newpolymap+probpolymap+newlanduse
      print(bigmap)
      mt_print(verbose, fname, "New polygon map, click on 'new_polygon' in the bottom right to inspect.")
      userchoice = readline(prompt = "Press [enter] to continue, or enter '-1' to undo the simplification: ")

      if(userchoice  == "-1"){
        mt_print(verbose, fname, "Undoing merge:",
                 paste("polygon", problem_poly$idx, "into polygon", merge_candiate$idx))
      }else if(userchoice == ""){
        to_simp <- to_simp_tenative
      }else{
        mt_print(verbose, fname, paste0("you entered: '", userchoice, "' >> this input is not recognized. Merge will be undone. "))
        mt_print(verbose, fname, "Undoing merge:",
                 paste("polygon", problem_poly$idx, "into polygon", merge_candiate$idx))
      }
    }else{
        to_simp <- to_simp_tenative
      }
  }
  mt_print(verbose, fname, "Finished checking all flags, returning dataset.")
  return(to_simp %>% select(-idx, -MILJOTOOLS_TYPE))
}

longest_shared_perimeter <- function(problem_poly, neigh){
  full <- rbind(problem_poly, neigh)
  # inspiration:
  # https://stackoverflow.com/questions/70869316/finding-ratio-of-perimeter-with-surrounding-neighbors-in-r

  # https://github.com/r-spatial/sf/issues/406
  sf::st_agr(full) = "constant"
  sf::st_agr(problem_poly) = "constant"

  full %>%
    sf::st_cast('MULTILINESTRING') %>%
    sf::st_intersection(problem_poly,) %>%
    dplyr::mutate(length_intersection = sf::st_length(.)) %>%
    dplyr::filter(length_intersection > units::as_units(0, 'm')) %>%
    dplyr::filter(idx != problem_poly$idx) %>% dplyr::arrange(dplyr::desc(length_intersection)) %>%
    sf::st_drop_geometry() %>% dplyr::pull(idx) %>% dplyr::first() -> result
  return(result)
}

