### MNRV3 NCDF Support
# Date: Sep. 26, 2024
# Author: Moritz Shore
# Purpose: Save the server request NCDF files as NCDF instead of converting immediately to dataframes. This is quite overcomplicated because there doesnt seem to be any functional way of opening the server requested files and then just writing them to disk.

#' Read and write ncdf files
#'
#' instead of coverting them to dataframe format
#'
#' @param url
#' @param savefiles
#'
#' @return
#' @keywords internal
#'
#' @examples
read_write_ncdf <- function(url, savefiles, directory, verbose){

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
    cat(blue("extracting"), underline(varlist[var_index]), blue("data"),"\n", sep = " ")
    nc_var_list[[var_index]] <- ncvar_get(ncin_crop, varlist[var_index])


    nc_var_list[[var_index]] %>% image(xlab= varlist[var_index], useRaster = T)
    # extract variable attributes
    cat(magenta("extracting"), underline(varlist[var_index]), magenta("attributes"),"\n", sep = " ")
    var_attr <- ncatt_get(ncin_crop, varlist[var_index])
    print(var_attr$standard_name)
    # define a variable defintion based on the extracted attributes
    # TODO could add chunk sizes which are present on variables that arent lat/long?
    cat(cyan("defining"), underline(varlist[var_index]), cyan("attributes"),"\n", sep = " ")
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
  cat(yellow("creating"), underline(filename), yellow("on disc"),"\n", sep = " ")
  to_write_nc <- nc_create(filename, vars = nc_attr_list)


  for (var_index in 1:varnr) {
    cat(green("writing"), underline(var_standard_name[[var_index]]), green("to file"),"\n", sep = " ")
    ncvar_put(nc = to_write_nc, varid = var_standard_name[[var_index]], vals = nc_var_list[[var_index]])
  }


  cat(yellow("saving"), underline(filename), yellow("data"),"\n", sep = " ")
  cat(yellow("closing file #"), underline(paste0(idate, "/", length(url))), "\n", sep = " ")
  nc_close(to_write_nc)
  nc_close(ncin_crop)

  }
  if(list.files(paste0(directory, foldername)) %>% length() == length(url)){
    cat(bold(bgGreen(">>> finished downloading:"),
             bgCyan(underline(white(paste(length(url), "files")))),
             bgGreen("<<<")), "\n", sep = "")

  }else{
    warning("it seems not all files were donwloaded!: ")
    cat(red(length(list.files(paste0(directory, foldername))), "/", length(url)), "\n")
  }

  }
