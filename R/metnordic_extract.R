#' MET Nordic Extract - Point Timeseries regional downloads
#'
#' This function extracts timeseries from (hourly) files from the chain
#' `metnordic_coordwindow()` -->  `metnordic_buildquery()` -->  `metnordic_coordmetnordic_download()` --> `metnordic_merge_hourly()` -->
#' `metnordic_extract()`. The function relies upon the `cmsafops` to extract values.
#'
#' @importFrom cmsafops selpoint
#' @importFrom readr read_delim
#'
#' @param directory (String) directory containing merged files as created by `metnordic_merge_hourly()`
#' @param mn_variables (vector, strings) variables to extract
#' @param lat (numeric) latitude of point to extract
#' @param lon (numeric) longitude of point to extract
#' @param outdir (string) directory in which to write the file (.csv)
#' @param name (string) name of the file (will be added to filename)
#' @param verbose (boolean) print?
#'
#' @returns path to written file
#' @export
#'
#'
metnordic_extract <-  function(directory, mn_variables, lat, lon, outdir, name, verbose = FALSE) {

  extract_var <- function(directory, variable, lat, lon, outdir) {
    infp = list.files(directory, variable, full.names = T)
    outfp = paste0(outdir, "TEMP_",variable, ".csv")
    cmsafops::selpoint(
      var = variable,
      infile = infp,
      outfile = outfp,
      lon1 = lon,
      lat1 = lat,
      format = "csv",
      verbose = verbose,
      overwrite = TRUE)
    # need to move things up by one day
    res = readr::read_delim(outfp, delim = ";", col_names = T, show_col_types = F)
    file.remove(outfp)
    colnames(res) <- c("date", variable)
    return(res)
  }

  extract_all_vars <- function(the_var) {
    extract_var(
      directory = directory,
      outdir = outdir,
      lat = lat,
      lon = lon,
      variable =  the_var
    )
  }

  reslist <- lapply(X = mn_variables, FUN = extract_all_vars)
  full_df <- do.call("cbind", reslist) %>% dplyr::select(all_of(c("date", mn_variables)))

  writefp <- paste0(outdir, name, "_metno-extract-LAT",lat %>%str_replace("\\.", "-"), "_LON",  lon %>%str_replace("\\.", "-"), ".csv")
  write_csv(full_df, file = writefp)
  return(writefp)
}
