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
#' @importFrom crayon bold bgGreen italic bgBlue bgYellow black bgCyan yellow underline
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
    f_theme <- bgCyan$black$bold

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
