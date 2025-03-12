nc_open_retry <- function(link) {

  nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                      error = function(cond){
                        warning("failed..")
                        return(NA)
                      })

  if(nc_file %>% length() > 1){
    return(nc_file)
  } else{
    print("retry download..")
    attempt = 1
    while((attempt < 10) & (length(nc_file) == 1)){
      Sys.sleep(5)
      attempt = attempt + 1
      nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                          error = function(cond){
                            return(NA)
                          })

    }

    if(length(nc_file) > 1){
      print("connection re-established!")
      return(nc_file)
    }else{
      warning("download failed after 10 attempts, returning NULL")
      return(NULL)
    }
  }
}
