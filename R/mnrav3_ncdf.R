### MNRV3 NCDF Support
# Date: Sep. 26, 2024
# Author: Moritz Shore
# Description: Code relating towards the manipulation of NCDF4 files without
# converting the data to dataframe / csv format.


#' Read and write ncdf files
#'
#' instead of coverting them to dataframe format
#'
#' @param url urls to download
#' @param savefiles paths to save
#' @importFrom ncdf4 ncdim_def ncvar_def ncvar_get ncatt_get ncvar_put nc_close nc_create
#' @importFrom crayon underline white red yellow underline bgGreen bgCyan bold magenta cyan
#' @importFrom dplyr  %>%
#' @return path to downloaded files
#' @keywords internal
#'
read_write_ncdf <- function(url, savefiles, directory, foldername, verbose = FALSE){

  # determine if any of the passed files have been downloaded already, and if
  # so, remove them from the "to download list"
  already_downloaded_files <- file.exists(savefiles)
  if(sum(already_downloaded_files) > 0){
    cat(red(underline(
      "miljotools thinks ",
      sum(already_downloaded_files),
      " files have already been downloaded, and will not try to re-download them..")))
    url <- url[-which(already_downloaded_files)]
    savefiles <- savefiles[-which(already_downloaded_files)]
  }

  # Download first file to get the dimensios set, then loop through following
  # Files
  idate = 1
  # open first Netcdf file
  ncin_crop <- nc_open_retry(url[idate])

  # get dimensions
  lat <- ncvar_get(ncin_crop, "y")
  lon <- ncvar_get(ncin_crop, "x")
  ydim <- dim(lat)
  xdim <- dim(lon)

  # defining dimensions
  mytimedef <- ncdim_def(name = "time", units = "hour", vals =1, unlim = F)
  myxdef <- ncdim_def(name = "y", units = "m",longname = "projection_y_coordinate",vals = 1:ydim)
  myydef <- ncdim_def(name = "x", units = "m",longname = "projection_x_coordinate", vals =1:xdim)

  # define UTMS
  lat_vals <- ncvar_def("lat_vals", units = "deg", dim = list(myydef,myxdef, mytimedef))
  lon_vals <- ncvar_def("lon_vals", units = "deg", dim = list(myydef,myxdef, mytimedef))

  # Start loop
  for (idate in c(1:length(url))) {
  ncin_crop <- nc_open_retry(url[idate])

  varlist = ncin_crop$var %>% names()
  varnr = length(varlist)
  nc_var_list = list()
  nc_attr_list = list()
  var_standard_name = list()

  for (var_index in 1:varnr) {

    # extract variable data
    if(verbose){
      cat(blue("extracting"), underline(varlist[var_index]), blue("data"),"\n", sep = " ")
      }

    nc_var_list[[var_index]] <- ncvar_get(ncin_crop, varlist[var_index])

    # extract variable attributes
    if(verbose){
      # plotting this may cause R session to abort?
      # nc_var_list[[var_index]] %>% image(xlab= varlist[var_index], useRaster = T)
      cat(magenta("extracting"), underline(varlist[var_index]), magenta("attributes"),"\n", sep = " ")}
    var_attr <- ncatt_get(ncin_crop, varlist[var_index])
    # define a variable defintion based on the extracted attributes
    # TODO could add chunk sizes which are present on variables that arent lat/long?
    if(verbose){cat(cyan("defining"), underline(varlist[var_index]), cyan("attributes"),"\n", sep = " ")}
    var_standard_name[[var_index]] <- var_attr$standard_name

    var_def <- ncvar_def(
      var_attr$standard_name,
      units = var_attr$units,
      list(myydef,myxdef, mytimedef)
    )

    # add the definition to the list
    nc_attr_list[[var_index]] <- var_def


  }
  names(nc_var_list) <- varlist
  names(nc_attr_list) <- varlist

  filename = savefiles[idate]
  # create NC file with the attribute definition list
  if(verbose){cat(yellow("creating"), underline(filename), yellow("on disc"),"\n", sep = " ")}
  to_write_nc <- nc_create(filename, vars = nc_attr_list)


  for (var_index in 1:varnr) {
    if(verbose){cat(green("writing"), underline(var_standard_name[[var_index]]), green("to file"),"\n", sep = " ")}
    ncvar_put(nc = to_write_nc, varid = var_standard_name[[var_index]], vals = nc_var_list[[var_index]])
  }


  if(verbose){cat(yellow("saving"), underline(filename), yellow("data"),"\n", sep = " ")}
  if(verbose){cat(yellow("closing file #"), underline(paste0(idate, "/", length(url))), "\n", sep = " ")}
  nc_close(to_write_nc)
  nc_close(ncin_crop)

  }
  if(list.files(paste0(directory, foldername)) %>% length() == length(url)){
    if(verbose){cat(bold(bgGreen(">>> finished downloading:"),
             bgCyan(underline(white(paste(length(url), "files")))),
             bgGreen("<<<")), "\n", sep = "")}

  }else{
    warning("it seems not all files were donwloaded!: ")
    cat(red(length(list.files(paste0(directory, foldername))), "/", length(url)), "\n")
    }
  }


#' converts parameters to CWATM format
#'
#' For CWatM climate variables required in netcdf format:#'
#'   - precipitation [Kg m-2 s-1], variable name = pr_nor2
#'   - temperature: max, min & average [K], variable name = tas_nor2, tasmax_nor2, tasmin_nor2
#'   - humidity (relative[%]), variable name = hurs_nor
#'   - surface pressure [Pa], variable name = ps_nor
#'   - radiation (short wave & long wave downwards) [W m-2], variable name = rsds_nor, rlds_nor,
#'   - windspeed [m/s], variable name = wind
#'
#'
#'
#' @param in
#' @param out
#'
#' @return
#' @export
#'
#' @examples
# convert_to_cwatm <- function(in, out){
#
# }
