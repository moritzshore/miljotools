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
    mt_print(TRUE, "nc_open_retry", "retrying donwload with out longwave radiation")

    # https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters
    # find the location in the link where longwave radiation is, and remove it,
    # then try to open the file wihtout this variable.
    split = link %>% stringr::str_split(",", simplify = T)
    longwave_index <- grepl(x = split, pattern =  "longwave") %>% which()
    new_link = paste(split[-longwave_index], collapse = ",")
    new_nc_file <- tryCatch(
      expr = {ncdf4::nc_open(new_link)},
      error = function(cond) {
        warning("failed..", cond, "retry!")
        return(NA)})
  }

  if (length(new_nc_file)>1) {
    mt_print(TRUE, "nc_open_retry", "download sans longradiation succeeded!")
    warning("file missing longwave radiation")
    return(new_nc_file)
  } else{
    mt_print(TRUE, "nc_open_retry", "retrying donwload..")

    attempt = 1
    while ((attempt < 10) & (length(nc_file) == 1)) {
      Sys.sleep(5)
      attempt = attempt + 1
      nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                          error = function(cond) {
                            warning("failed..", cond, "retry!")
                            return(NA)})
    }

    if (length(nc_file) > 1) {
      mt_print(TRUE, "nc_open_retry", "connection re-established!")
      return(nc_file)
    } else{
      stop("download failed after 10 attempts.")
    }
  }
}
