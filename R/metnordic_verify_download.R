"CWATM/metnordic/download_prec_temp/" -> folder
setwd("../met-nordic-paper/")

#' Verify MET Nordic download
#'
#' This function scans the download folder for files that are "too small" and
#' then deletes them. You can then use the `download_metnordic_daterange()` with
#' the same directory (and queries from `metnordic_build_query()`) function to
#' re-download the deleted ones
#'
#' @param folder
#'
#' @returns Nothing
#' @export
#'
#' @examples
#'
#' # TODO
metnordic_verify_download <- function(folder) {

  # TODO: move the verify code from the merge_hourly to this function.
  # you should first check the file sizes, and then if thats ok run the "in-order" code.

  list.files(folder, pattern = ".nc", full.names = T) -> myfiles
  getfilesizes <- function(myfile) {
    file.info(myfile)$size %>% return()
  }
  mt_print(TRUE, "metnordic_verify_download", text = "Scanning files...")
  all_sizes <- lapply(myfiles, getfilesizes) %>% unlist()
  mt_print(TRUE, "metnordic_verify_download", text = "Finished scanning.")
  bar = mean(all_sizes) - mean(all_sizes) * .5
  myfiles[which(all_sizes < bar)] -> small_files

  if (length(small_files) > 0) {
    mt_print(TRUE,
             "metnordic_verify_download",
             text = "the following files were flagged for redownload..\n > ",
             small_files %>% paste(collapse = "\n > "))

    mt_print(TRUE,
             "metnordic_verify_download",
             text = "Deleting these files now..",
             "(please redownload)")

    file.remove(small_files) -> status

    if (status %>% all()) {
      mt_print(
        TRUE,
        "metnordic_verify_download",
        text = "All files deleted successfully",
        "Please re-run `metnordic_download_daterange`"
      )
    } else{
      warning("Not all files were removed...")
    }
  } else{
    mt_print(TRUE, "metnordic_verify_download", text = "No files were flagged for redownload!")
  }
}
