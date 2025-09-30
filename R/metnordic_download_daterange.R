#' Download MET Nordic files
#'
#' Downloads a list of provided queries from the MET No thredds server using the
#' OPENDAP protocol. The queries should be passed from
#' `metnordic_buildqueries()`. Folder of where to download, and which variables
#' to download also need to be provided! Any files already present in the folder
#' will not be re-downloaded. This means that if the download fails for whatever
#' network reason, you can just restart the function and it should pick off
#' where it left off. If you are having issues with downloads, make sure to
#' check https://status.met.no/ for server (THREDDS) status.
#'
#' @param queries list as passed by `metnordic_buildqueries()`
#' @param directory folder where to download
#' @param mn_variables MET Nordic variables to download.
#' @param verbose print status?
#'
#' @returns path to download directory
#' @export
#'
#' @seealso [metnordic_buildquery()]
metnordic_download_daterange <- function(queries, directory, mn_variables, verbose = FALSE) {
  # these are all the urls that should be downloaded
  urls <- queries$full_urls
  # this is where the files will be written
  dir.create(directory, showWarnings = F)
  # these are the files that have already been written
  if(list.files(directory) %>% length() > 0){
    already_downloaded <- paste0((list.files(directory) %>% str_split("Z_", simplify = T))[, 1] %>% unique(), "Z.nc")
    # these are the files that then should not be re-downloaded
    dont_redownload <- queries$filenames %in% already_downloaded %>% which()
    # these are the files that should be downloaded
    remaining_urls <- urls[-dont_redownload]
    # this is how many have already been downloaded (only for printing)
    i = length(dont_redownload)
  }else{
    remaining_urls <- urls
    i = 1
  }
  # this is how many still need downloading
  ix = length(urls)
  # downloading each file in a foreloop
  for (url in remaining_urls) {
    # TODO convert to mt_print()
    if(verbose){cat(paste0("\rdownloading ...[", i, "/", ix, "] >> ", queries$filenames[i]))}
    return = metnordic_download(
      url = url,
      outdir = directory,
      vars = mn_variables,
      overwrite = F,
      verbose = F
    )
    i = i + 1
  }
  if(verbose){cat("\rDownload Finished.")}

  return(directory)
}
