#' Thermopluviogram
#'
#' Combines climate data from [Norsk Klima Service
#' Center](https://klimaservicesenter.no/) and observed data to produce
#' ["Thermopluviograms"](https://www.dwd.de/EN/ourservices/jahresthermo/jahresthermo.html).
#'
#' https://www.senorge.no/PrecTempMap
#'
#' todo: describe file struture
#'
#' Generates thermopluviograms and extreme rain index plots from NVE
#' not flexible. only works with current source data structure.
#'
#' Date:  01.06.2022
#' Author:  Moritz Shore
#'
#' @param modelled_climate filepath of climate files (i.e. "Climate_Data/Modelled/")
#' @param observed_pr filepath of observed precipitation data (.csv)
#' @param observed_tm filepath of the observed mean temperature data (.csv)
#' @param outpath outpath for plotting
#' @param location location of plot (is used for file naming) (string)
#' @param ref_startdate reference period start (ie."1971-01-01")
#' @param ref_enddate reference period d end (ie. "2005-12-31")
#' @param obs_startdate observation period start (ie. "2041-01-01")
#' @param obs_enddate observation period end (ie. "2070-12-31")
#' @param verbose print progress?
#' @param left_yaxis_only If set to true, only the historical plot axis will be labeled (This is so the three plots can be placed next to each other in a figure)
#' @param chosen_model_runs Names of the models to color as blue
#' @param experiments Experiments to generate (ie. c("hist", "rcp45","rcp85"))
#' @param fixed_axis use a fixed axis range when plotting? (boolean)
#' @param axis_font_size Font size (Integer)
#' @param yaxisrange y axis range (ie. c(800,1100), mm)
#' @param xaxisrange x axis range (ie.  c(4.5, 8.5), Celsius)
#' @param xlab  x axis label. (ie. "Mean Annual Temperature)", u00B0 is degree symbol)
#'
#' @return path to generated files
#' @importFrom dplyr %>% select filter group_by summarise if_else
#' @export
#'
#'
thermopluviogram <- function(modelled_climate,
                             observed_pr,
                             observed_tm,
                             outpath,
                             location,
                             ref_startdate,
                             ref_enddate,
                             obs_startdate,
                             obs_enddate,
                             verbose = TRUE,
                             chosen_model_runs = NULL,
                             experiments = c("hist", "rcp45","rcp85"),
                             left_yaxis_only = FALSE,
                             fixed_axis = TRUE,
                             axis_font_size = 7,
                             yaxisrange = c(800, 1100),
                             xaxisrange = c(4.5, 8.5),
                             xlab = "Mean Annual Temperature (\u00B0C)") {

  # fs is a silly dependency and should be removed.. TODO
  required_packages <- c("tibble", "fs", "ggrepel", "lubridate", "vroom")
  install_missing_packs(required_packages)

  ## Setup
#
  ## testing par set
  # modelled_climate  = "C:/Users/mosh/Documents/GIT/thermopluviograms/Climate_Data/Modelled/"
  # observed_pr =  "C:/Users/mosh/Documents/GIT/thermopluviograms/Climate_Data/Observed/senorge_pr.csv"
  # observed_tm =  "C:/Users/mosh/Documents/GIT/thermopluviograms/Climate_Data/Observed/senorge_temp.csv"
  #
  # outpath = "C:/Users/mosh/Documents/thermoplots"
  #
  # location = "testingmt"
  #
  # ref_startdate = "1971-01-01"
  # ref_enddate = "2005-12-31"
  # obs_startdate = "2041-01-01"
  # obs_enddate = "2070-12-31"
  # fixed_axis = T
  # axis_font_size = 7
  # yaxisrange = c(800,1100)
  # xaxisrange = c(4.5, 8.5)
  # xlab = "Mean Annual Temperature (\u00B0C)"
  # chosen_model_runs = NULL
  #
  # verbose = TRUE
  # chosen_model_runs = NULL
  # experiments = c("hist", "rcp45","rcp85")
  # left_yaxis_only = FALSE
  # fixed_axis = TRUE
  # axis_font_size = 7
  # yaxisrange = c(800, 1100)
  # xaxisrange = c(4.5, 8.5)
  # xlab = "Mean Annual Temperature (\u00B0C)"

  # years of the reference period, in vector form
  ref_daterange.y = lubridate::year(seq.Date(
    from = as.Date(ref_startdate),
    to = as.Date(ref_enddate),
    by = "year"
  ))
  # years of the observation period, in vector form
  obs_daterange.y = lubridate::year(seq.Date(
    from = as.Date(obs_startdate),
    to = as.Date(obs_enddate),
    by = "year"
  ))

  # days of the reference period, in vector form
  ref_daterange.d = (seq.Date(
    from = as.Date(ref_startdate),
    to = as.Date(ref_enddate),
    by = "day"
  ))
  # days of the observation period, in vector form
  obs_daterange.d = (seq.Date(
    from = as.Date(obs_startdate),
    to = as.Date(obs_enddate),
    by = "day"
  ))

  if(is.null(chosen_model_runs)){
    chosen_model_runs = c("CNRM-CCLM", "CNRM-RCA", "ECEARTH-RCA", "HADGEM-RCA", "IPSL-RCA")
  }

  status_print <- paste("\n...for period", obs_startdate, "to", obs_enddate,
                        "\n...with a reference period from", ref_enddate, "to", ref_startdate,
                        "\n...for location named", location,
                        "\n...using modelled data from", modelled_climate,
                        "\n...and observed precipitation data from", observed_pr,
                        "\n...and observed temperature data from", observed_tm,
                        "\n...and generating for the experiments", (experiments %>% paste(collapse = ", ")),
                        "\n...with an x axis range from", xaxisrange[1], "to", xaxisrange[2], "labelled:", xlab,
                        "\n...with a y axis range from ", yaxisrange[1], "to", yaxisrange[2], "and plotting only the leftmost label?", left_yaxis_only,
                        "\n...using a font size of ", axis_font_size,
                        "\n...with a fixed axis?", fixed_axis,
                        "\n...highlighting models:", (chosen_model_runs %>% paste(collapse = ", ")),
                        "\n...and generating files here", outpath
                        )

  mt_print(verbose, "thermopluviogram", "generating thermopluviograms...", status_print)
  mt_print(verbose, "thermopluviogram", "importing data...")

  total_data <- tpg_import_data(
    modelled_climate = modelled_climate,
    observed_pr_path = observed_pr,
    observed_tm_path = observed_tm,
    ref_daterange.d = ref_daterange.d,
    verbose = verbose
  )

  mt_print(verbose, "thermopluviogram", "calculating statisics...")
  stat_calc <- tpg_calc_stat(
    total_data = total_data,
    ref_daterange.y = ref_daterange.y,
    obs_daterange.y = obs_daterange.y,
    chosen_model_runs = chosen_model_runs,
    verbose = verbose
  )

  mt_print(verbose, "thermopluviogram", "generating plots...")
  returnplot <- tpg_plot(
    experiments = experiments,
    ref_startdate = ref_startdate,
    ref_enddate = ref_enddate,
    obs_startdate = obs_startdate,
    obs_enddate = obs_enddate,
    per_stat = stat_calc$per_stat,
    xtreme_rain = stat_calc$xtreme_rain,
    location = location,
    outpath = outpath,
    chosen_model_runs = chosen_model_runs,
    xaxisrange = xaxisrange,
    axis_font_size = axis_font_size,
    verbose = verbose,
    yaxisrange = yaxisrange,
    xlab = xlab,
    left_yaxis_only = left_yaxis_only
  )

  returnplot %>% return()
}

tpg_import_data <- function(modelled_climate, observed_pr_path, observed_tm_path, ref_daterange.d, verbose){
  # Modelled data import
  mt_print(verbose, "thermopluviogram", "loading modelled data from ", modelled_climate)
  # read in file paths of the chosen location (bad code)
  modelled_data <- modelled_climate %>% fs::dir_ls() %>%
    vroom::vroom(id = "path", col_names = F, show_col_types = F) # use column "path" as an ID column. ignore column names
  # remove the column names that were imported as rows (bad code)
  modelled_data <- modelled_data[which(modelled_data$X1!="date"),]

  # name columns correctly (bad code)
  colnames(modelled_data) <- c("path", "date", "hist", "rcp45", "rcp85")

  mt_print(verbose, "thermopluviogram", "generating property matrix...")
  # generate a property matrix
  properties <- modelled_data$path %>%
    stringr::str_remove(modelled_climate) %>% # remove the path from the ID string
    stringr::str_remove("_DAY.csv") %>% # remove the suffix
    stringr::str_split(pattern = "_", simplify = T) # split based on the "_"

  mt_print(verbose, "thermopluviogram", "post-processing modelled climate")

  # apply properties
  modelled_data$GCM <- properties[,1] # apply GCM
  modelled_data$RCM <- properties[,2] # apply RCM
  modelled_data$VAR <- properties[,3] # apply variable
  modelled_data$MODEL <- paste0(modelled_data$GCM, "-", modelled_data$RCM) # apply model name
  modelled_data$TYPE = "MOD" # apply data type "MODelled"

  # force datatype to numeric
  modelled_data$hist <- as.numeric(modelled_data$hist)
  modelled_data$rcp45 <- as.numeric(modelled_data$rcp45)
  modelled_data$rcp85 <- as.numeric(modelled_data$rcp85)
  modelled_data$date <- as.Date(modelled_data$date)

  # drop the path/ID column now that its been parsed
  modelled_data <- modelled_data %>% dplyr::select(-1) # -1 removes the first column

  # Measured Data Import
  # import observed data from senorge
  mt_print(verbose, "thermopluviogram", "loading observed precipitation data from ", observed_pr_path)
  observed_pr <- vroom::vroom(observed_pr_path, comment = "#",na= "65535", show_col_types = F)

  mt_print(verbose, "thermopluviogram", "loading observed temperature data from ", observed_tm_path)
  observed_tm <- vroom::vroom(observed_tm_path, comment = "#", na = "65535", show_col_types = F)

  mt_print(verbose, "thermopluviogram", "post-processing observed data")

  # force correct date format
  observed_tm$Date <-as.Date(observed_tm$Date, format = "%d.%m.%Y")
  observed_pr$Date <-as.Date(observed_pr$Date, format = "%d.%m.%Y")

  # merge two measurements
  observed <- dplyr::left_join(x = observed_tm, y = observed_pr,by="Date")

  # fix column names
  colnames(observed) <- c("date", "tm", "pr")

  observed$tm = observed$tm + 273.15

  # filter in the dates within the observed period.
  observed_cropped = observed %>% dplyr::filter(date %in% ref_daterange.d)

  # create a compatible dataframe to merged with modeled (TM)
  # why are we putting the measured value in for all three experiments?
  # so that we can compare them to all three experiments (but really one should only compare to HIST)
  # and for ease of use as well, keeping the system constant.
  observed_tm_df <-
    tibble::tibble(
      date = observed_cropped$date,
      hist = observed_cropped$tm,
      rcp45 = observed_cropped$tm,
      rcp85 = observed_cropped$tm, # why are we putting this value in for all three experiments?
      GCM = "MEA", # filler value, as there is no real GCM for measured
      RCM = "MEA",  # filler value, as there is no real rcm for measured
      VAR = "TM", # TM is the id for precipitation
      MODEL = "Measured", # this is being added as a defact "model run" with the name "Measured"
      TYPE = "MEA" # the type of value is MEAsured, as opposed to MODelled.
    )

  # create a compatible dataframe to merged with modeled (RR)
  observed_rr_df <-
    tibble::tibble(
      date = observed_cropped$date,
      hist = observed_cropped$pr,
      rcp45 = observed_cropped$pr,
      rcp85 = observed_cropped$pr,
      GCM = "MEA",
      RCM = "MEA",
      VAR = "RR",
      MODEL = "Measured",
      TYPE = "MEA"
    )

  mt_print(verbose, "thermopluviogram", "merging observed and modelled data")

  # combine modeled and observed data
  # the observed data is being treated as if it were just another model run!
  total_data <- rbind(modelled_data, observed_tm_df, observed_rr_df)

  # add a year column for statistics
  total_data <- total_data %>%
    dplyr::mutate(YEAR = lubridate::year(date))

  return(total_data)

}

tpg_calc_stat <- function(total_data, ref_daterange.y, obs_daterange.y, chosen_model_runs, verbose){
  # sum by model and year

  # RMD check appeastment
  .data <- YEAR <- MODEL <- sum_precip_hist <- mean_temp_hist <-  sum_precip_rcp45   <-
    mean_temp_rcp45  <- sum_precip_rcp85 <- mean_temp_rcp85  <- VAR <- hist_rr <- hist_tm <- rcp45_rr <- rcp45_tm  <-
    rcp85_rr <- rcp85_tm <- NULL

  # precip
  precip_year <- total_data %>% # take the full dataframe
    dplyr::filter(.data$VAR == "RR") %>% # Filter only precipitation
    dplyr::group_by(.data$YEAR, .data$MODEL) %>% # Group by year and model
    dplyr::summarise(sum_precip_hist = sum(.data$hist, na.rm = T), # Sum per year per model
              sum_precip_rcp45 = sum(.data$rcp45, na.rm = T),
              sum_precip_rcp85 = sum(.data$rcp85, na.rm = T))

  # Manually replace the 0 values with NA (Why?)
  precip_year$sum_precip_hist[which(precip_year$sum_precip_hist==0)] = NA
  precip_year$sum_precip_rcp45[which(precip_year$sum_precip_rcp45==0)] = NA
  precip_year$sum_precip_rcp85[which(precip_year$sum_precip_rcp85==0)] = NA

  # temp
  tm_year <- total_data %>%
    dplyr::filter(.data$VAR == "TM") %>%
    dplyr::group_by(.data$YEAR, .data$MODEL) %>%
    dplyr::summarise(mean_temp_hist = mean(.data$hist, na.rm = T),
              mean_temp_rcp45 = mean(.data$rcp45, na.rm = T),
              mean_temp_rcp85 = mean(.data$rcp85, na.rm = T))

  # and recombine (havent figuired out a better way to do it)
  yearly_stat <- cbind(tm_year, precip_year[c(3,4,5)])

  # mean into total time period (in this case 30years) by model for hist
  refper_stat <- yearly_stat %>% dplyr::filter(YEAR %in% ref_daterange.y) %>%
    dplyr::group_by(MODEL) %>% # only group by model this time, thus collapsing the 30 year time periods.
    dplyr::summarise(sum_precip  = mean(sum_precip_hist, na.rm = T),
              mean_temp= mean(mean_temp_hist, na.rm = T)-273.15) # convert back into C from kelvin

  # mean into total time period (in this case 30years) by model for rcp45
  obsper_stat_rcp45 <- yearly_stat %>% dplyr::filter(YEAR %in% obs_daterange.y) %>%
    dplyr::group_by(MODEL) %>% # only group by model this time, thus collapsing the 30 year time periods.
    dplyr::summarise(sum_precip = mean(sum_precip_rcp45, na.rm = T),
              mean_temp = mean(mean_temp_rcp45, na.rm = T)-273.15)

  # add the quasi-historical model run "measured" onto the bottom of this
  obsper_stat_rcp45 <- rbind(obsper_stat_rcp45, refper_stat[which(refper_stat$MODEL=="Measured"),])

  # mean into total time period (in this case 30years) by model for rcp85
  obsper_stat_rcp85 <- yearly_stat %>% dplyr::filter(YEAR %in% obs_daterange.y) %>%
    dplyr::group_by(MODEL) %>%
    dplyr::summarise(sum_precip = mean(sum_precip_rcp85, na.rm = T),
              mean_temp = mean(mean_temp_rcp85, na.rm = T)-273.15)
  # add the quasi-historical model run "measured" onto the bottom of this
  obsper_stat_rcp85 <- rbind(obsper_stat_rcp85, refper_stat[which(refper_stat$MODEL=="Measured"),])

  # add the experiments as labels
  refper_stat$EXP = "hist"
  obsper_stat_rcp85$EXP = "rcp85"
  obsper_stat_rcp45$EXP = "rcp45"

  # and recombine (havent figured out a better way to do this)
  per_stat <- rbind(refper_stat, obsper_stat_rcp45, obsper_stat_rcp85)
  per_stat$chosen = dplyr::if_else(per_stat$MODEL %in% chosen_model_runs, "chosen", "not chosen")
  per_stat$chosen[which(per_stat$MODEL == "Measured")] = "Measured"

  # extreme rain statistics calc

  total_data_rr <-total_data %>% dplyr::filter(VAR == "RR")
  total_data_tm <- total_data %>% dplyr::filter(VAR == "TM")

  colnames(total_data_rr) <- c("date", "hist_rr", "rcp45_rr", "rcp85_rr", "GCM", "RCM", "VAR", "MODEL", "TYPE", "YEAR")

  total_data_tm <- total_data_tm[c(2,3,4)]
  colnames(total_data_tm) <- c("hist_tm", "rcp45_tm", "rcp85_tm")

  total_data_bc <- cbind(total_data_rr, total_data_tm)
  xtreme_rain <- total_data_bc %>%
    dplyr::group_by(MODEL) %>%
    dplyr::summarise(hist = length(which(hist_rr>25 & hist_tm > 273.15)),
              rcp45 =length(which(rcp45_rr>25 & rcp45_tm > 273.15)),
              rcp85 =length(which(rcp85_rr>25 & rcp85_tm > 273.15)),
    )


  return_list <- list(per_stat = per_stat, xtreme_rain = xtreme_rain)

  return_list %>% return()
}

tpg_plot <- function(experiments,
                     ref_startdate,
                     ref_enddate,
                     obs_startdate,
                     obs_enddate,
                     per_stat,
                     xtreme_rain,
                     location,
                     outpath,
                     chosen_model_runs,
                     xaxisrange,
                     axis_font_size,
                     verbose,
                     yaxisrange,
                     xlab,
                     left_yaxis_only) {
  # RCMD check appeasement
  EXP <- . <- mean_temp <- sum_precip <- chosen <- MODEL <- NULL

  custom_axis_theme <- ggplot2::theme(
    legend.position = "none",
    axis.title.x = ggplot2::element_text(colour = "black", size = axis_font_size),
    axis.title.y = ggplot2::element_text(colour = "black", size = axis_font_size),
    axis.text.x = ggplot2::element_text(colour = "black", size = axis_font_size),
    axis.text.y = ggplot2::element_text(colour = "black", size = axis_font_size),
    plot.title = ggplot2::element_text(colour = "black", size = axis_font_size),
    panel.grid.major.y = ggplot2::element_line(color = "grey70", linewidth = .5),
    panel.grid.major.x = ggplot2::element_line(color = "grey70", linewidth = .5),
    panel.grid.minor.y = ggplot2::element_line(color = "grey90", linewidth = .5),
    panel.grid.minor.x = ggplot2::element_line(color = "grey90", linewidth = .5)
  )

  print_range <- paste0(
    stringr::str_sub(lubridate::year(as.Date(ref_startdate)),3,4),"-",
    stringr::str_sub(lubridate::year(as.Date(ref_enddate)),3,4),"_",
    stringr::str_sub(lubridate::year(as.Date(obs_startdate)),3,4),"-",
    stringr::str_sub(lubridate::year(as.Date(obs_enddate)),3,4)
  )

  # for every TPG, do:
  for (current_experiment in experiments) {


    # current_experiment = experiments[2]
    # Initial setup

    # define the "print format" of the current experiment.
    if(current_experiment=="hist"){print_experiment = "historical"}
    if(current_experiment=="rcp45"){print_experiment = "RCP 4.5"}
    if(current_experiment=="rcp85"){print_experiment = "RCP 8.5"}

    mt_print(verbose, "thermopluviogram", "generating thermopluviogram for experiment", print_experiment)

    # format the reference time range
    timerange = paste0(lubridate::year(ref_startdate), " to ", lubridate::year(ref_enddate))

    # format the observed time range
    fut_timerange =  paste0(lubridate::year(obs_startdate), " to ", lubridate::year(obs_enddate))

    # remove measured data for any non hist plot
    if(current_experiment != "hist"){per_stat2 <-  per_stat[-which(per_stat$MODEL=="Measured"),]}else{per_stat2 = per_stat}

    # create plot with only the data of the current experiment
    plot <- per_stat2 %>% dplyr::filter(EXP == current_experiment) %>% ggplot2::ggplot(data = .) +

      # Scatter plot points
    ggplot2::geom_point(
      ggplot2::aes(mean_temp , sum_precip , fill = chosen),
      color = "grey20",
      stroke = .2,
      shape = 23,
      size = 2
    ) +

      # Repelled labels
    ggrepel::geom_label_repel(
      mapping = ggplot2::aes(mean_temp, sum_precip, label = stringr::str_replace(MODEL, pattern = "-", replacement = "\n"),
                    color = chosen,
                    segment.square  = FALSE, # line curvature settings
                    segment.inflect = FALSE), # line curvature settings
      label.size = .1, # Size of label border, in mm.
      inherit.aes = T,
      size = 2, # size of label
      fontface = "bold",
      max.overlaps = 100, # amount of labels that are allowed to overlap
      max.iter = 900000, # max attempts
      box.padding = .4, # space between boxes
      point.padding = .2, # space between point and arrow
      label.padding = .1, # space between label text and border
      min.segment.length = 0.0, # minimum length for an arrow
      force = 1000,
      segment.size  = 0.4, # arrow line thickness
      segment.curvature = 0, # line curvature
      force_pull = .02,
      arrow = ggplot2::arrow(type = "open", angle = 20, length = ggplot2::unit(.14, "cm")),
      direction = "both",
      verbose = T
    )+

      # Axis limits and titles
    ggplot2::ggtitle(print_experiment) +
      ggplot2::xlim(xaxisrange) +
      ggplot2::ylim(c(yaxisrange)) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab("Mean Total Annual Precipitation (mm)") +

      # custom labels and colors for the border
    ggplot2::scale_color_manual(name = "Chosen",
                       breaks = c("chosen", "not chosen", "Measured"),
                       values = c("chosen" = 'deepskyblue3',"not chosen" = "grey30","Measured" = "yellow4"))+

      # custom labels and colors for the fill
    ggplot2::scale_fill_manual(name = "Chosen",
                      breaks = c("chosen", "not chosen", "Measured"),
                      values = c("chosen" = 'deepskyblue3',"not chosen" = "grey30", "Measured" = "yellow4"))+
      # base theme
    ggplot2::theme_bw()+

      # Theme adjustments
    custom_axis_theme

    # remove y axis if not the leftmost graph in the paper
    if(current_experiment != "hist" && left_yaxis_only){
      plot <- plot + ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }


    tpg_filename =  paste0("tpg_",location,"_",current_experiment,"_",print_range,".png")
    mt_print(verbose, "thermopluviogram", "saving thermopluviogram ", tpg_filename)

    # save plot
    ggplot2::ggsave(plot = plot, filename =tpg_filename,
           device = "png",
           path = outpath,
           height = 5.33,
           width = 5.33,
           units = "cm"
    )


    #### Extreme Rain
    mt_print(verbose, "thermopluviogram", "generating exetreme rain plot for experiment ", current_experiment)


    # BIOWATER CHOSEN MODELS/VARIABLES
    xtreme_rain$chosen = dplyr::if_else(xtreme_rain$MODEL %in% chosen_model_runs, "chosen", "not chosen")
    xtreme_rain$chosen[which(xtreme_rain$MODEL == "Measured")] = "Measured"


    # remove measured for now (the shorter timespan means too little events)
    if(current_experiment != "hist"){xtreme_rain2 <-  xtreme_rain[-11,]}else{xtreme_rain2 <- xtreme_rain}

    # Autodetect y lim
    # TODO make this better
    max_yvaL <- c(xtreme_rain2$hist, xtreme_rain2$rcp85, xtreme_rain2$rcp85) %>% max()
    autoylim <- round(max_yvaL/100, 0) * 100


    title = "Occurances of Extreme Rainfall"
    subtitle = paste0(location, ", ", timerange, ". Scenario: " ,current_experiment)
    yaxis = "Extreme Rainfall Events"
    # the plot
    xtreme_plot <- xtreme_rain2 %>% ggplot2::ggplot(ggplot2::aes(x = stats::reorder(MODEL, get(current_experiment)), y = get(current_experiment),color = "black") )+ggplot2::theme_bw()+
      ggplot2::geom_col(ggplot2::aes(fill = chosen),position = "dodge", width = .7)+
      ggplot2::ylab(yaxis)+
      # TODO replace this with par?
      ggplot2::ylim(c(0,autoylim))+
      ggplot2::scale_fill_manual(name = "",
                        breaks = c("chosen", "not chosen", "Measured"),
                        values = c("chosen" = 'deepskyblue3', "not chosen" = "gray30", "Measured"= "yellow4"))+
      ggplot2::scale_color_manual(values = "black")+


      custom_axis_theme+

      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust =.4, colour = "black", size = 7),
            legend.position = "none",
            axis.title.x = ggplot2::element_blank()
      )


    if(current_experiment != "hist" && left_yaxis_only){
      xtreme_plot <- xtreme_plot + ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank())}

    xtreme_filename<-paste0("xtreme_rain_",location,"_",current_experiment,"_",print_range,".png")
    mt_print(verbose, "thermopluviogram", "saving extreme rain plot", xtreme_filename)

    ggplot2::ggsave(
      plot = xtreme_plot,
      filename =xtreme_filename,
      device = "png",
      path = outpath,
      height = 5.33,
      width = 5.33,
      units = "cm"
    )
  }

  mt_print(verbose, "thermopluviogram", "plots have been saved to", outpath)

  return(outpath)
}
