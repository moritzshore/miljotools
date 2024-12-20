# TODO: something clever needs to be done here about the solar radiation issue.
# TODO: this version does not work very well...
# nc_open_stable <- function(link) {
#
#   # Try to open the NC file...
#   nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
#                       error = function(cond){
#                         warning("failed..")
#                         return(NA)
#                       })
#
#   # if the NC file was successfully opened...
#   if (nc_file %>% length() > 1) {
#     # return it...
#     return(nc_file)
#     # If not....
#   } else{
#     # Retry the download...
#     mt_print(TRUE, "nc_open_retry", crayon::red("retrying donwload.."))
#     attempt = 1
#     # a maxmimum of 10 times
#     while ((attempt < 10) & (length(nc_file) == 1)) {
#       mt_print(TRUE, "nc_open_retry", crayon::red("retrying donwload.. #"), attempt)
#       # and wait 10 seconds inbetween each attempt.
#       Sys.sleep(10)
#       attempt = attempt + 1
#       # Try once more to open the file
#       nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
#                           error = function(cond) {
#                             warning("failed again..", cond, "retry #", attempt)
#                             return(NA)})
#     }
#
#     # Check if the download succeeded after the final attempt:
#     if (length(nc_file) > 1) {
#       # if it did, return
#       mt_print(TRUE, "nc_open_retry", "connection re-established!")
#       return(nc_file)
#     } else{
#       # if it didnt, return a NULL and give a warning
#       warning("download failed after 10 attempts..")
#       return(NULL)
#     }
#   }
# }


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
                            warning("failed..", cond, "retry!")
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

# a second attempt that might be more stable
nc_open_retry_v2 <- function(link, vars) {

  # This attempts to connect to the server. if it works, the file is returned, if it does not, NA is returned.
  nc_file <- tryCatch(expr = {ncdf4::nc_open(link)},
                      error = function(cond){
                        warning("connection to link failed: ",link)
                        return(NA)
                      })

  # if the nc_file opened successfully, then test if we can access a variable
  if(nc_file$error == FALSE){
    nc_var <- tryCatch(expr = {ncdf4::ncvar_get(nc_file, varid = vars[1])},
                        error = function(cond){
                          warning("accesing variable", vars[1], "failed in file:\n", link)
                          return(NA)
                        })
    # if the nc_var was accessed succesfully, it should be a matrix, ie.
    # numeric. if this is the case, it should be safe to return. if it did not
    # work, we return a after sleeping for a bit (to give the client time to
    # reconnect?)
    if (nc_var %>% is.numeric()) {
      return(nc_file)
    }
    else{
      # if the access to the variable failed.. wait a bit and then return NULL.
      Sys.sleep(10)
      warning(">>> Access to variable failed for\n >>", vars[1], "\nin url:\n\n",
              link,
              "\n\n>>> Returning NULL, Please retry download!")
      return(NULL)
    }
  }else{
    # if the connection to the file itself failed.. wait a bit and then return
    # NULL.
    Sys.sleep(10)
    warning(">>> Connection failed for url:\n vvvv     \n",
            link,
            "\n ^^^^     \n>>> Returning NULL, Please retry download!")
    return(NULL)
  }
}
