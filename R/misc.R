#' custom print function for miljotools
#'
#' @param verbose print or not
#' @param function_name function name string
#' @param text  string 1
#' @param text2  string 2
#' @param rflag in place replacement flag
#'
#' @return nothing
#' @keywords internal
#'
#' @importFrom crayon bold bgGreen italic bgBlue bgYellow black bgCyan yellow underline bgWhite cyan white
#'
#'
#'
mt_print <- function(verbose, function_name, text, text2 = NULL, rflag = FALSE) {
  miljotheme <- bold$bgGreen
  tools_theme <- bold$italic$bgBlue

  if (function_name == "cwatm_hourly_to_daily_ncdf4") {
    f_theme  <- bgYellow$black$bold
  }else if(function_name == "thermopluviogram"){
    f_theme = bgWhite$cyan$bold
  } else{
    f_theme <- bgCyan$white$bold

  }
  text_theme <- italic$yellow
  text_2_theme <- black $ underline

  if(rflag){
    prefix = "\r"
    suffix = NULL
  }else{
    prefix = NULL
    suffix = "\n"
  }
  if (verbose) {
    cat(
      prefix,
      miljotheme("miljo"),
      bgBlue("\U1F33F"),
      tools_theme("tools "),
      f_theme(paste0("", function_name, "")),
      text_theme(" >>", text, ""),
      text_2_theme(text2),
      suffix,
      sep = ""
    )
  }
}


install_missing_packs <- function(required_packages) {
  missing_packs <- which((required_packages %in% utils::installed.packages()) == FALSE)
  if(length(missing_packs) > 0){
    mt_print(TRUE, "", "Missing CRAN packages are required to run this function, installing now:",
            paste0(required_packages[missing_packs], sep = " "))
    utils::install.packages(required_packages[missing_packs])
  }
  if(length(which((required_packages %in% utils::installed.packages()) == FALSE)) > 0){
    stop("following packages failed to install:\n",
         required_packages[which((required_packages %in% utils::installed.packages()) == FALSE)])
  }
}
