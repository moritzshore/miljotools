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
#' @param verfiy Logical: verify the existence of all required files? (recommended to be TRUE)
#' @param n_cores Numeric: max number of cores to perform operation with. (optional)
#' @param verbose Logical: print to console? (optional)
#'
#' @returns Returns a named list with file paths to each aggregated file. `FALSE`
#'   indicates a file failed to be aggregated, likely due to not having 24hrs
#'   (24 individual files) to aggregated from.
#' @export
#'
#' @importFrom stringr str_remove_all
#' @importFrom dplyr  %>%
#'
#' @seealso [metnordic_aggregate()] [metnordic_download_daterange()] [metnordic_merge_daily()]
#'
metnordic_aggregate_daterange <- function(directory,
                                          variable,
                                          method,
                                          start,
                                          end,
                                          outpath,
                                          overwrite = TRUE,
                                          verify = FALSE,
                                          n_cores = NULL,
                                          verbose = TRUE) {
  # Creating date range from start to end dates
  daterange = seq(from  = start %>% as.Date(), to = end %>% as.Date())
  # converting it into the correct format (THIS MIGHT NOT BE STABLE DEPENDING ON LOCALE?)
  daterange %>% stringr::str_remove_all("-") -> dayformat

  if(verify){
    mt_print(TRUE, "metnordic_aggregate_daterange", "[verify = TRUE]", "Scanning for missing files...")
    short_fps <- list.files(directory, pattern = "*.nc")
    short_fps_filt <- short_fps[(grepl(x = short_fps, pattern = variable) %>% which())]

    short_fps_filt %>% str_remove(variable) %>% str_remove("_.nc") %>% str_split("_", simplify = T) -> set1
    set1[,dim(set1)[2]] -> dtcode
    year = substr(dtcode, 0,4)
    month = substr(dtcode, 5,6)
    day = substr(dtcode, 7,8)
    hour = substr(dtcode, 10,11)
    paste0(year, "-", month, "-", day, " ", hour, ":00:00") -> dates

    dates %>% range() %>% as.POSIXct(tz = "UTC")  -> daterange
    seq.POSIXt(daterange[1], daterange[2],by = "hour") %>% strftime(tz = "UTC")-> fulldate
    which(dates != fulldate) -> issues

    if(length(dates) != length(fulldate)){
      mt_print(TRUE, "metnordic_aggregate_daterange", "[verify = TRUE]", "Missing dates detected!")
      stop("Issue detected in timerange: Missing hour between [", dates[issues %>% first()-1], "] and [", dates[issues %>% first()], "]",
           "\n\n", "You are missing the following timestamp: [", fulldate[issues %>% min()], "] for [", variable, "]",
           "\n\n", "Please download the file AFTER: [", short_fps_filt[issues %>% min()-1], "] (+1 hour) and rerun..")
    }else{
      if(issues %>% length() > 1){
        mt_print(TRUE, "metnordic_aggregate_daterange", "[verify = TRUE]", "Missing dates detected!")
        stop(paste0("missing dates: ",dates[issues], collapse = "\n"))
      }else{
        mt_print(TRUE, "metnordic_aggregate_daterange", "[verify = TRUE]",  "No missing dates detected")
      }
    }
  }

  # Lapply ready version
  custom_agg <- function(current_day){
    if(overwrite == FALSE){
      list.files(outpath, pattern = variable) -> xs
      which(grepl(pattern = current_day,x = xs)) -> match
      if(length(match) > 0){
        grepl(pattern = method, x = xs[match]) -> check_true
        return(paste0(current_day," ", variable, " ", method, " SKIPPED, ALREADY EXISTS"))
      }
    }
    miljotools::metnordic_aggregate(
      directory = directory,
      variable = variable,
      method = method,
      day = current_day,
      outpath = outpath,
      overwrite = overwrite,
      verbose = FALSE
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
  result <- foreach(day = dayformat) %dopar% {custom_agg(current_day = day)}
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
