# TODO: something clever needs to be done here about the solar radiation issue.
nc_open_stable <- function(link) {

  # Try to open the NC file...
  nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                      error = function(cond){
                        warning("failed..")
                        return(NA)
                      })

  # if the NC file was successfully opened...
  if (nc_file %>% length() > 1) {
    # return it...
    return(nc_file)
    # If not....
  } else{
    # Retry the download...
    mt_print(TRUE, "nc_open_retry", crayon::red("retrying donwload.."))
    attempt = 1
    # a maxmimum of 10 times
    while ((attempt < 10) & (length(nc_file) == 1)) {
      mt_print(TRUE, "nc_open_retry", crayon::red("retrying donwload.. #"), attempt)
      # and wait 10 seconds inbetween each attempt.
      Sys.sleep(10)
      attempt = attempt + 1
      # Try once more to open the file
      nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                          error = function(cond) {
                            warning("failed again..", cond, "retry #", attempt)
                            return(NA)})
    }

    # Check if the download succeeded after the final attempt:
    if (length(nc_file) > 1) {
      # if it did, return
      mt_print(TRUE, "nc_open_retry", "connection re-established!")
      return(nc_file)
    } else{
      # if it didnt, return a NULL and give a warning
      warning("download failed after 10 attempts..")
      return(NULL)
    }
  }
}
