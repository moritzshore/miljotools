# TODO: something clever needs to be done here about the solar radiation issue.
nc_open_stable <- function(link) {

  nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                      error = function(cond){
                        warning("failed..")
                        return(NA)
                      })


  if (nc_file %>% length() > 1) {
    return(nc_file)
  } else{
    mt_print(TRUE, "nc_open_retry", crayon::red("retrying donwload.."))
    attempt = 1
    while ((attempt < 10) & (length(nc_file) == 1)) {
      Sys.sleep(2)
      attempt = attempt + 1
      nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                          error = function(cond) {
                            warning("failed again..", cond, "retry #", attempt)
                            return(NA)})
    }
    if (length(nc_file) > 1) {
      mt_print(TRUE, "nc_open_retry", "connection re-established!")
      return(nc_file)
    } else{
      warning("download failed after 10 attempts..")
    }
  }
}
