#' Aggregate a date range
#'
#' A wrapper function for `metnordic_aggregate()` which aggregates files from a
#' start to end date (in parallel). You must supply a variable type and a method
#' of aggregation (eg. mean). This function is designed to take input from
#' `metnordic_download_daterange()` and provides input for
#' `metnordic_merge_daily()`
#'
#' Currently supported are the following types of aggregation: "mean" "min"
#' "max" "sum". Operations are performed in paralell on multiple cores. You can
#' control the number of cores used with the `num_cores` parameters.
#'
#' @param directory String: Path to the source files (as downloaded by `metnordic_download()` or `metnordic_download_daterange()`)
#' @param variable String: MET Nordic variable to aggregate (eg. "precipitation_amount", [(see more)](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters))
#' @param method String: method of aggregation ("mean", "min", "max", "sum")
#' @param start String: start of the date range to aggregate (eg. "2015-01-01")
#' @param end String: end of the date range to aggregate (eg. "2015-12-31")
#' @param outpath String: path to directory of to be created files
#' @param overwrite Logical: overwrite existing files? (optional, default TRUE)
#' @param n_cores Numeric: max number of cores to perform operation with. (optional)
#' @param verbose Logical: print to console? (optional)
#'
#' @returns Returns a named list with file paths to each aggregated file. `FALSE`
#'   indicates a file failed to be aggregated, likely due to not having 24hrs
#'   (24 individual files) to aggregated from.
#' @export
#'
#' @seealso [metnordic_aggregate()] [metnordic_download_daterange()] [metnordic_merge_daily()]
#'
metnordic_aggregate_daterange <- function(directory, variable, method, start, end, outpath, overwrite = TRUE, n_cores = NULL, verbose = TRUE){
  # Creating date range from start to end dates
  daterange = seq(from  = start %>% as.Date(), to = end %>% as.Date())
  # converting it into the correct format (THIS MIGHT NOT BE STABLE DEPENDING ON LOCALE?)
  daterange %>% str_remove_all("-") -> dayformat
  # Lapply ready version
  custom_agg <- function(current_day){
    metnordic_aggregate(
      directory = directory,
      variable = variable,
      method = method,
      day = current_day,
      outpath = outpath,
      overwrite = overwrite,
      verbose = verbose
    )
  }
  # determining number of cores
  if(is.null(n_cores)){
    n_cores = parallel::detectCores() - 2
    if(n_cores > length(dayformat)){
      n_cores = length(dayformat)
    }
  }
  # performing operation in parallel
  mt_print(verbose, "metnordic_aggregate_daterange", paste("Aggregating", length(dayformat), "days"), paste0("On ", n_cores, " threads."))
  cl <-parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  result <- foreach(day = dayformat, .packages = "miljotools") %dopar% {custom_agg(current_day = day)}
  parallel::stopCluster(cl)
  # naming the return list by date
  names(result) <- dayformat
  # print message depends on if any failed. (Which is normal if the start or end did not have 24hrs)
  if((result %>% unname() %>% unlist() == "FALSE") %>% any()){
    mt_print(verbose, "metnordic_aggregate_daterange", "Finished. returning list with file names.", "(List elements containing 'FALSE' failed.)")
  }else{
    mt_print(verbose, "metnordic_aggregate_daterange", "Finished. returning list with file names.")
  }
  # returning data
  return(result)
}
