mt_print <- function(verbose, function_name, text, text2 = NULL, rflag = FALSE) {
  miljotheme <- bold$bgGreen
  tools_theme <- bold$italic$bgBlue

  if (function_name == "cwatm_hourly_to_daily_ncdf4") {
    f_theme  <- bgYellow$black$bold
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
