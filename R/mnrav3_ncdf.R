### MNRV3 NCDF Support
# Date: Sep. 26, 2024
# Author: Moritz Shore
# Description: Code relating towards the manipulation of NCDF4 files without
#              converting the data to dataframe / csv format.


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

  # extra printing for debug purposes
  DEBUG = FALSE

  # determine if any of the passed files have been downloaded already, and if
  # so, remove them from the "to download list"
  already_downloaded_files <- file.exists(savefiles)
  if(sum(already_downloaded_files) > 0){
    cat(red(underline(
      ">>> miljotools thinks ",
      sum(already_downloaded_files),
      " files have already been downloaded, and will not try to re-download them..\n")))
    url <- url[-which(already_downloaded_files)]
    savefiles <- savefiles[-which(already_downloaded_files)]
  }

  # Download first file to get the dimensions set, then loop through following
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
    if(verbose){cat("\r", yellow("downloading file #"), underline(paste0(idate, "/", length(url))), sep = "")}
    ncin_crop <- nc_open_retry(url[idate])

  varlist = ncin_crop$var %>% names()
  varnr = length(varlist)
  nc_var_list = list()
  nc_attr_list = list()
  var_standard_name = list()

  for (var_index in 1:varnr) {

    # extract variable data
    if(DEBUG){
      cat(blue("extracting"), underline(varlist[var_index]), blue("data"),"\n", sep = " ")
      }

    nc_var_list[[var_index]] <- ncvar_get(ncin_crop, varlist[var_index])

    # extract variable attributes
    if(DEBUG){
      # plotting this may cause R session to abort?
      # nc_var_list[[var_index]] %>% image(xlab= varlist[var_index], useRaster = T)
      cat(magenta("extracting"), underline(varlist[var_index]), magenta("attributes"),"\n", sep = " ")}
    var_attr <- ncatt_get(ncin_crop, varlist[var_index])
    # define a variable defintion based on the extracted attributes
    # TODO could add chunk sizes which are present on variables that arent lat/long?
    if(DEBUG){cat(cyan("defining"), underline(varlist[var_index]), cyan("attributes"),"\n", sep = " ")}
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
  if(DEBUG){cat(yellow("creating"), underline(filename), yellow("on disc"),"\n", sep = " ")}
  to_write_nc <- nc_create(filename, vars = nc_attr_list)


  for (var_index in 1:varnr) {
    if(DEBUG){cat(green("writing"), underline(var_standard_name[[var_index]]), green("to file"),"\n", sep = " ")}
    ncvar_put(nc = to_write_nc, varid = var_standard_name[[var_index]], vals = nc_var_list[[var_index]])
  }


  if(DEBUG){cat(yellow("saving"), underline(filename), yellow("data"),"\n", sep = " ")}
  if(DEBUG){cat(yellow("closing file #"), underline(paste0(idate, "/", length(url))), "\n", sep = " ")}
  nc_close(to_write_nc)
  nc_close(ncin_crop)


  }
  if(list.files(paste0(directory, foldername)) %>% length() == length(url)){
    if(verbose){cat(bold(bgGreen("\n>>> finished downloading:"),
             bgCyan(underline(white(paste(length(url), "files")))),
             bgGreen("<<<")), "\n", sep = "")}

  }else{
    warning("it seems not all files were donwloaded!: ")
    cat(red(length(list.files(paste0(directory, foldername))), "/", length(url)), "\n")
    }
  }


#' converts parameters to CWATM format
#'
#' For CWatM climate variables required in netcdf format:
#'   - precipitation [Kg m-2 s-1], variable name = pr_nor2
#'   - temperature: max, min & average [K], variable name = tas_nor2, tasmax_nor2, tasmin_nor2
#'   - humidity (relative[%]), variable name = hurs_nor
#'   - surface pressure [Pa], variable name = ps_nor
#'   - radiation (short wave & long wave downwards) [W m-2], variable name = rsds_nor, rlds_nor,
#'   - windspeed [m/s], variable name = wind
#'
#'   You need to pass the following hourly MetNo Variables to this function:
#'
#'    "precipitation_amount" (mm)
#'    "air_temperature_2m" (K)
#'    "relative_humidity_2m" (%)
#'    "air_pressure_at_sea_level" (pa)
#'    "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time" (W/m^2 s)*
#'    "integral_of_surface_downwelling_longwave_flux_in_air_wrt_time"  (W/m^2 s)**
#'    "wind_speed_10m"
#'
#'    (*) "integral_of_Y_wrt_X" means int Y dX. The data variable should have an
#'    axis for X specifying the limits of the integral as bounds. "wrt" means
#'    with respect to. The surface called "surface" means the lower boundary of
#'    the atmosphere. "shortwave" means shortwave radiation. Downwelling
#'    radiation is radiation from above. It does not mean "net downward".
#'    Surface downwelling shortwave is the sum of direct and diffuse solar
#'    radiation incident on the surface, and is sometimes called "global
#'    radiation". When thought of as being incident on a surface, a radiative
#'    flux is sometimes called "irradiance". In addition, it is identical with
#'    the quantity measured by a cosine-collector light-meter and sometimes
#'    called "vector irradiance". In accordance with common usage in geophysical
#'    disciplines, "flux" implies per unit area, called "flux density" in
#'    physics.
#'
#'    (**) This variable has missing data for most of 2014, 2016, and 14 hours
#'    in 2019 (see Known data issues)
#'
#'     source: https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset
#'     Date Accessed: 06-10-2024
#'
#'
#'
#'
#' @param in filepath to folder containing .nc input files
#' @param out filepath to write CWatM input files
#'
#' @importFrom stringr str_split str_remove
#' @importFrom dplyr %>%
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom abind abind
#' @importFrom purrr map
#'
#' @return status code
#' @export
#'
#' @examples
convert_to_cwatm <- function(inpath, outpath){

  # parse the files (to get the daily timestep)
  filepath_full <- list.files(inpath, full.names = T)
  parsed <- (list.files(inpath) %>% str_split("_", simplify = T))[,6] %>% str_remove(".nc")
  date_only <- (parsed %>% str_split("T", simplify = T))[,1] %>% unique()

  # for every day in the time range, upscale the data in the temporal dimension
  # (hourly to daily)
  for (today in date_only) {

    # Create an output directory
    dir.create(outpath)

    print(today)
    today_file_index <- grepl(x = parsed, pattern = today) %>% which()

    # if less than 24 files are found that means some are missing
    if(today_file_index %>% length() < 24){
      if(today == dplyr::first(date_only)){
        Warning("first day of download is incomplete, skipping this day")
        next()
      }
      else if(today == dplyr::last(date_only)){
        Warning("last day of download is incomplete, skipping this day")
        next()
      }
      stop("the following date is missing some files! [>> ", today, " <<]")
    }

    # open all the files at once
    files_nc <- map(filepath_full[today_file_index], nc_open)

    # extract one to use as a template
    template_nc <- files_nc[[1]]
    ### loading the vars
    # Why not vectorized? because different operations need to be performed on
    # the various met variables.


    pr_stack <- map(files_nc, varid = "precipitation_amount", .f = ncvar_get) %>% abind(along = 3)
    ta_stack <- map(files_nc, varid = "air_temperature", .f = ncvar_get) %>% abind(along = 3)
    rh_stack <- map(files_nc, varid = "relative_humidity", .f = ncvar_get) %>% abind(along = 3)
    ap_stack <- map(files_nc, varid = "air_pressure_at_sea_level", .f = ncvar_get) %>% abind(along = 3)
    ds_stack <- map(files_nc, varid = "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time", .f = ncvar_get) %>% abind(along = 3)
    dl_stack <- map(files_nc, varid = "integral_of_surface_downwelling_longwave_flux_in_air_wrt_time", .f = ncvar_get) %>% abind(along = 3)
    ws_stack <- map(files_nc, varid = "wind_speed", .f = ncvar_get) %>% abind(along = 3)

    # for precipitation: sum up along time dimension
    tot_daily_precip <- rowSums(pr_stack, dims = 2)
    # Temperature: take min max and average
    t_max_daily <- apply(ta_stack, MARGIN = c(1, 2), FUN = max)
    t_min_daily <- apply(ta_stack, MARGIN = c(1, 2), FUN = min)
    t_avg_daily <- rowMeans(ta_stack, dims = 2)
    # for RH: take the average
    rh_avg_daily <- rh_stack %>% rowMeans(dims = 2)
    # for surface pressure: average
    sp_avg_daily <- ap_stack %>% rowMeans(dims = 2)
    # for downwelling long and shortwave: sum and add?
    ds_sum <- rowSums(ds_stack, dims = 2)
    dl_sum <- rowSums(dl_stack, dims = 2)
    tot_rad_daily <- dl_sum + ds_sum
    # for windpseed: daily mean?
    avg_ws_daily <- rowMeans(ws_stack, dims = 2)

    nc_fp <- paste0(outpath, "/temp_cwatm_prep_", today, ".nc")
    opened_nc <- nc_create(nc_fp, vars =template_nc$var)



    cwatm_vars <- c(
      "precipitation_amount",
      "air_temperature",
      "relative_humidity",
      "air_pressure_at_sea_level",
      "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",
      "integral_of_surface_downwelling_longwave_flux_in_air_wrt_time",
      "wind_speed"
    )



    ncvar_put(nc = opened_nc, varid = cwatm_vars, vals = )


  }
}
