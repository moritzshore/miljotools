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
#' @export
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



#' Estimate Pressure at Altitude
#'
#' Solves for 'p', Static pressure at altitude 'h' in N m-2 assuming a linear
#' temperature gradient. Modified from Walter Bislin's [write up](http://walter.bislins.ch/bloge/index.asp?page=Barometric+Formula).
#'
#' \deqn{p = p_\mathrm{ref}\, \left( {T_\mathrm{ref} + \alpha \cdot (h - h_\mathrm{ref}) \over T_\mathrm{ref}} \right) ^ {-\beta} = p_\mathrm{ref}\, \left( 1 + {\alpha \cdot (h - h_\mathrm{ref}) \over T_\mathrm{ref}} \right) ^ {-\beta}}
#'
#' with: \eqn{\beta = {M \cdot g \over R \cdot \alpha} = { g \over R_\mathrm{S} \cdot \alpha}}
#'
#' where:
#'
#' \eqn{p}	= Static pressure at altitude h in N m-2
#'
#' \eqn{p_\mathrm{ref}} = Static pressure at reference altitude href. For href = 0 m (sea level), pref = 101,325 Pa
#'
#' \eqn{T_\mathrm{ref}} = Temperature at reference altitude href. For href = 0 m (sea level), Tref = 288.15 K (15 °C)
#'
#' \eqn{\alpha} = Temperature gradient (negative lapse rate) = −0.0065 K -m
#'
#' \eqn{h} = Altitude above sea level in meter from 0 m up to 11,000 m
#'
#' \eqn{h_\mathrm{ref}} = Reference altitude. For sea level href = 0 m
#'
#' \eqn{M} = Molar mass; for dry air = 28.9644 g/mol
#'
#' \eqn{g} = Mean gravitational acceleration at sea level = 9.80665 m/s2
#'
#' \eqn{R} = Universal Gas Constant = 8.31446 J/(mol·K)
#'
#' \eqn{R_\mathrm{S}} = Specific Gas Constant; dry air = 287.058 J/(kg·K)
#'
#' To calculate \eqn{T_\mathrm{ref}} we need to use the lapse rate to estimate
#' `talt`. This can be done with the following formula:
#'
#' \deqn{T(h) = T_\mathrm{ref} + \alpha_i\, (h-h_\mathrm{ref})}
#'
#' where:
#'
#' \eqn{T(h)} = Temperature at altitude h in Kelvin
#'
#' \eqn{T_\mathrm{ref}} = Temperature at a reference altitude (e.g. sea level)
#'
#' \eqn{\alpha_\mathrm{i}} = Temperature Gradient (negative Lapse Rate) of the i-th layer in K/m (Kelvin per meter)
#'
#' \eqn{h} = Altitude in m
#'
#' \eqn{h_\mathrm{ref}} = Refrence altitude (sea level = 0 m)
#'
#' @param h altitude above sea level in meters
#' @param pref static pressure at reference altitude (`href`) Unit: Pa
#' @param talt temperature at  altitude (`h`) Unit: Kelvin
#' @param alpha Temperature gradient (negative lapse rate) Unit: K m^-1
#' @param href Reference altitude. Unit: m
#' @param g Mean gravitational acceleration at sea level. Unit: m s^-2
#' @param Rs Specific gas constant of dry air J (kg*K)^-1
#' @returns Static pressure at altitude `h` in N m^2
#' @export
#'
#' @author Walter Bislin [(walter.bislins.ch)](http://walter.bislins.ch/), adapted by Moritz Shore
#'
#' @examples
#' #TODO
calc_pres_at_alt <- function(h,
                             talt,
                             pref = 101325,
                             alpha = -0.0065,
                             href = 0,
                             g = 9.80665,
                             Rs = 287.058) {
  # # reference temperature calculation:
  tref = talt - (alpha*(h-href))

  # beta calculation:
  negbeta = (g/(Rs*alpha))*-1

  # p calculation:

  top = alpha*(h-href)
  bottom = tref
  frac = top/bottom
  to_exp = 1+frac
  exped = to_exp^negbeta

  p = pref * exped

  return(p)
}


calc_pres_at_alt(h = 92, talt = 288.15)

