#' Thermopluviograms
#'
#' Generates a thermopluviogram from 'climate' data based on a reference and an
#' observational period. Can include 'measured' data.
#'
#'
#' This function was designed to work with downscaled, bias-adjusted EURO-CORDEX
#' data from NVE:
#'
#'  https://nedlasting.nve.no/klimadata/kss
#'
#'  Downscaling and Bias-adjustment by Wong et al. (2016). The EURO-CORDEX
#'  dataset is from Jacob et al. (2014)
#'
#'  Function designed to work With variables average temperature
#'  'Gjennomsnittstemperatur' (x) and 'Nedbør' (y). Data range is from
#'  1971-2100, and supports any model and RCP4.5, RCP8.5
#'
#'
#' **References**
#'
#' Wong, W. K., Haddeland, I., Lawrence, D., & Beldring, S. (2016). Gridded 1 x
#' 1 km climate and hydrological projections for Norway.
# ' http://hdl.handle.net/11250/2500569
#'
#' Jacob, D., Petersen, J., Eggert, B. et al. EURO-CORDEX: new high-resolution
#' climate change projections for European impact research. Reg Environ Change
#' 14, 563–578 (2014). https://doi.org/10.1007/s10113-013-0499-2
#'
#' @param climate filepath to the directory of the climate data (required)
#' @param ref_startdate reference period start date (ie. "1981-01-01")
#' @param ref_enddate reference period end date (ie. "2010-12-31")
#' @param obs_startdate observation period start date (ie. "2041-01-01")
#' @param obs_enddate observation period end date (ie. "2070-12-31")
#' @param measured filepath to the measured data (optional)
#' @param name name of the plot (optional)
#' @param axis_font_size
#' @param output_path directory path of the generated plots (default: working directory)
#' @param fixed_axis Use a fixed axis for all plots? (Default: FALSE)
#' @param y_axis_range Y-axis range when generating a fixed axis plot (ie. c(0,100))
#' @param x_axis_range X-axis range when generating a fixed axis plot (ie. c(0,100))
#' @param xlab custom X-axis label (optional)
#' @param ylab custom Y-axis label (optional)
#' @param custom_theme custom plot theme (GGplot theme object) (optional)
#'
#' @return Filepath to generated thermopluviogram
#' @export
#'
#' @examples
thermopluviogram <-
  function(climate,
           measured = NULL,
           ref_startdate = "1981-01-01",
           ref_enddate = "2010-12-31",
           obs_startdate = "2041-01-01",
           obs_enddate = "2070-12-31",
           location = "Location X",
           output_path = getwd(),
           fixed_axis = TRUE,
           axis_font_size = 7,
           y_axis_range = c(800,1100),
           x_axis_range = c(4.5, 8.5),
           xlab = "Mean Annual Temperature (\u00B0C)",
           ylab = "Mean Total Annual Precipitation (mm)",
           custom_theme = NULL){

  }


# main

# Author:  Moritz Shore
# Date:  01.06.2022
# Purpose: Generates thermopluviograms and extreme rain index plots from NVE
# data (includes measured data)
# note: not flexible. only works with current source data structure.

# Libraries ----
{
  library(fs) # file system access
  library(dplyr) # keeping things tidy
  library(lubridate) # date wrangling
  library(vroom) # speed and power!
  library(ggrepel) # label placement
  library(ggplot2) # plotting
  #library(ggpubr) # could implement
  library(reshape2)
  library(stringr)
}

source("settings.R")

source("import_data.R")

source("statistics.R")

source("plotting.R")

print("main finsihed")

## This is just legacy support stuff. Renaming the input parameters to their\
## original script names, instead of doing find/replace.

# Settings
# location of climate files
# Paths -- this could be modified to support multiple locations.
#modelled_climate_data_path = "Climate_Data/Modelled/"
modelled_climate_data_path = input

# location of observed data
# obs_file        = "Climate_Data/Observed/observed_data_aas.csv"
obs_file = measured

# outpath for plotting
# outpath = "Plots/"
outpath = output_path

# location of plot. is used for file naming
 location = name


# reference period start and end
# ref_startdate = "1971-01-01"
# ref_enddate = "2005-12-31"

# observation period start and end
# obs_startdate = "2041-01-01"
# obs_enddate = "2070-12-31"

# use a fixed axis range when plotting ?
#fixed_axis = T

# Font size
# axis_font_size = 7

# y axis
yaxisrange = y_axis_range # y axis range

# X axis
xaxisrange = x_axis_range # x axis range


xlab = "Mean Annual Temperature (\u00B0C)" # \u00B0 is degree symbol


if(custom_theme %>% is.null()){
  # GGPLOT axis theme settings
  custom_axis_theme <- theme(
    legend.position = "none",
    axis.title.x = element_text(colour = "black", size = axis_font_size),
    axis.title.y = element_text(colour = "black", size = axis_font_size),
    axis.text.x = element_text(colour = "black", size = axis_font_size),
    axis.text.y = element_text(colour = "black", size = axis_font_size),
    plot.title = element_text(colour = "black", size = axis_font_size),
    panel.grid.major.y = element_line(color = "grey70", linewidth = .5),
    panel.grid.major.x = element_line(color = "grey70", linewidth = .5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = .5),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = .5)
  )
}else{
  custom_axis_theme <- custom_theme
}


# date formatting ----
  # string form of date range for for file name identification
  print_range <- paste0(
    str_sub(year(as.Date(ref_startdate)),3,4),"-",
    str_sub(year(as.Date(ref_enddate)),3,4),"_",
    str_sub(year(as.Date(obs_startdate)),3,4),"-",
    str_sub(year(as.Date(obs_enddate)),3,4)
  )

  # years of the reference period, in vector form
  ref_daterange.y = year(seq.Date(from = as.Date(ref_startdate), to = as.Date(ref_enddate), by = "year"))
  # years of the observation period, in vector form
  obs_daterange.y = year(seq.Date(from = as.Date(obs_startdate), to = as.Date(obs_enddate), by = "year"))

  # days of the reference period, in vector form
  ref_daterange.d = (seq.Date(from = as.Date(ref_startdate), to = as.Date(ref_enddate), by = "day"))
  # days of the observation period, in vector form
  obs_daterange.d = (seq.Date(from = as.Date(obs_startdate), to = as.Date(obs_enddate), by = "day"))

# import data

# Modelled data import ----

# read in file paths of the chosen location
modelled_data <- modelled_climate_data_path %>% dir_ls() %>%
  vroom(id = "path", col_names = F) # use column "path" as an ID column. ignore column names

# remove the column names that were imported as rows
modelled_data <- modelled_data[which(modelled_data$X1!="date"),]

# name columns correctly
colnames(modelled_data) <- c("path", "date", "hist", "rcp45", "rcp85")

# generate a property matrix
properties <- modelled_data$path %>%
  str_remove(modelled_climate_data_path) %>% # remove the path from the ID string
  str_remove("_DAY.csv") %>% # remove the suffix
  str_split(pattern = "_", simplify = T) # split based on the "_"

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
modelled_data <- modelled_data %>% select(-1) # -1 removes the first column

# Measured Data Import ----


# import observed data from senorge
observed_tm <- vroom("Climate_Data/Observed/senorge_temp.csv", comment = "#",na= "65535")
observed_pr <- vroom("Climate_Data/Observed/senorge_pr.csv", comment = "#", na = "65535")

# force correct date format
observed_tm$Date <-as.Date(observed_tm$Date, format = "%d.%m.%Y")
observed_pr$Date <-as.Date(observed_pr$Date, format = "%d.%m.%Y")


# merge two measurements
observed <- left_join(x = observed_tm, y = observed_pr,by="Date")


# h-o data
hoimport <- vroom("Climate_Data/Observed/ass_met_1993-2019.csv")
hoimport$Date <-as.Date(hoimport$Date, format = "%d.%m.%Y")

library(stringr)
hoimport$pr <- str_replace_all(hoimport$pr, pattern = ",", replacement = ".")
hoimport$pr <- as.numeric(hoimport$pr)

hoimport$tmean <- str_replace_all(hoimport$tmean, pattern = ",", replacement = ".")
hoimport$tmean <- as.numeric(hoimport$tmean)

observed <- data.frame(date = hoimport$Date, tm = hoimport$tmean, pr = hoimport$pr)

# fix column names
colnames(observed) <- c("date", "tm", "pr")

observed$tm = observed$tm + 273.15

# filter in the dates within the observed period.
observed_cropped = observed %>% filter(date %in% ref_daterange.d)

# create a compatible dataframe to merged with modeled (TM)
# why are we putting the measured value in for all three experiments?
# so that we can compare them to all three experiments (but really one should only compare to HIST)
# and for ease of use as well, keeping the system constant.
observed_tm_df <-
  tibble(
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
  tibble(
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

# combine modeled and observed data
# the observed data is being treated as if it were just another model run!
total_data <- rbind(modelled_data, observed_tm_df, observed_rr_df)

# add a year column for statistics
total_data <- total_data %>%
  mutate(YEAR = lubridate::year(date))

print("imported data")


# Calculate statistics  NEED TO DOCUMENT, LEFT OFF HERE

# sum by model and year

# precip
precip_year <- total_data %>% # take the full dataframe
  filter(VAR == "RR") %>% # Filter only precipitation
  group_by(YEAR, MODEL) %>% # Group by year and model
  summarise(sum_precip_hist = sum(hist, na.rm = T), # Sum per year per model
            sum_precip_rcp45 = sum(rcp45, na.rm = T),
            sum_precip_rcp85 = sum(rcp85, na.rm = T))

# Manually replace the 0 values with NA (Why?)
precip_year$sum_precip_hist[which(precip_year$sum_precip_hist==0)] = NA
precip_year$sum_precip_rcp45[which(precip_year$sum_precip_rcp45==0)] = NA
precip_year$sum_precip_rcp85[which(precip_year$sum_precip_rcp85==0)] = NA

# temp
tm_year <- total_data %>%
  filter(VAR == "TM") %>%
  group_by(YEAR, MODEL) %>%
  summarise(mean_temp_hist = mean(hist, na.rm = T),
            mean_temp_rcp45 = mean(rcp45, na.rm = T),
            mean_temp_rcp85 = mean(rcp85, na.rm = T))

# and recombine (havent figuired out a better way to do it)
yearly_stat <- cbind(tm_year, precip_year[c(3,4,5)])

# mean into total time period (in this case 30years) by model for hist
refper_stat <- yearly_stat %>% filter(YEAR %in% ref_daterange.y) %>%
  group_by(MODEL) %>% # only group by model this time, thus collapsing the 30 year time periods.
  summarise(sum_precip  = mean(sum_precip_hist, na.rm = T),
            mean_temp= mean(mean_temp_hist, na.rm = T)-273.15) # convert back into C from kelvin

# mean into total time period (in this case 30years) by model for rcp45
obsper_stat_rcp45 <- yearly_stat %>% filter(YEAR %in% obs_daterange.y) %>%
  group_by(MODEL) %>% # only group by model this time, thus collapsing the 30 year time periods.
  summarise(sum_precip = mean(sum_precip_rcp45, na.rm = T),
            mean_temp = mean(mean_temp_rcp45, na.rm = T)-273.15)

# add the quasi-historical model run "measured" onto the bottom of this
obsper_stat_rcp45 <- rbind(obsper_stat_rcp45, refper_stat[which(refper_stat$MODEL=="Measured"),])

# mean into total time period (in this case 30years) by model for rcp85
obsper_stat_rcp85 <- yearly_stat %>% filter(YEAR %in% obs_daterange.y) %>%
  group_by(MODEL) %>%
  summarise(sum_precip = mean(sum_precip_rcp85, na.rm = T),
            mean_temp = mean(mean_temp_rcp85, na.rm = T)-273.15)
# add the quasi-historical model run "measured" onto the bottom of this
obsper_stat_rcp85 <- rbind(obsper_stat_rcp85, refper_stat[which(refper_stat$MODEL=="Measured"),])

# add the experiments as labels
refper_stat$EXP = "hist"
obsper_stat_rcp85$EXP = "rcp85"
obsper_stat_rcp45$EXP = "rcp45"

# and recombine (havent figured out a better way to do this)
per_stat <- rbind(refper_stat, obsper_stat_rcp45, obsper_stat_rcp85)
# BIOWATER CHOSEN MODELS/VARIABLES
chosen_model_runs = c("CNRM-CCLM", "CNRM-RCA", "ECEARTH-RCA", "HADGEM-RCA", "IPSL-RCA")
per_stat$chosen = if_else(per_stat$MODEL %in% chosen_model_runs, "chosen", "not chosen")
per_stat$chosen[which(per_stat$MODEL == "Measured")] = "Measured"


# extreme rain statistics calc

total_data_rr <-total_data %>% filter(VAR == "RR")
total_data_tm <- total_data %>% filter(VAR == "TM")

colnames(total_data_rr) <- c("date", "hist_rr", "rcp45_rr", "rcp85_rr", "GCM", "RCM", "VAR", "MODEL", "TYPE", "YEAR")

total_data_tm <- total_data_tm[c(2,3,4)]
colnames(total_data_tm) <- c("hist_tm", "rcp45_tm", "rcp85_tm")

total_data_bc <- cbind(total_data_rr, total_data_tm)
xtreme_rain <- total_data_bc %>%
  group_by(MODEL) %>%
  summarise(hist = length(which(hist_rr>25 & hist_tm > 273.15)),
            rcp45 =length(which(rcp45_rr>25 & rcp45_tm > 273.15)),
            rcp85 =length(which(rcp85_rr>25 & rcp85_tm > 273.15)),
  )



print("statistics calculated")


# TPGs to generate
current_experiments = c("hist", "rcp45","rcp85")
# for every TPG, do:
for (current_experiment in current_experiments) {

  # Initial setup ----

  # define the "print format" of the current experiment.
  if(current_experiment=="hist"){print_experiment = "historical"}
  if(current_experiment=="rcp45"){print_experiment = "RCP 4.5"}
  if(current_experiment=="rcp85"){print_experiment = "RCP 8.5"}

  # format the reference time range
  timerange = paste0(year(ref_startdate), " to ", year(ref_enddate))

  # format the observed time range
  fut_timerange =  paste0(year(obs_startdate), " to ", year(obs_enddate))

  # remove measured data for any non hist plot
  if(current_experiment != "hist"){per_stat2 <-  per_stat[-which(per_stat$MODEL=="Measured"),]}else{per_stat2 = per_stat}

  # create plot with only the data of the current experiment  -----
  plot <- per_stat2 %>% filter(EXP == current_experiment) %>% ggplot(data = .) +

    # Scatter plot points  -----
  geom_point(
    aes(mean_temp , sum_precip , fill = chosen),
    color = "grey20",
    stroke = .2,
    shape = 23,
    size = 2
  ) +

    # Repelled labels  -----
  geom_label_repel(
    mapping = aes(mean_temp, sum_precip, label = str_replace(MODEL, pattern = "-", replacement = "\n"),
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
    arrow = arrow(type = "open", angle = 20, length = unit(.14, "cm")),
    direction = "both",
    verbose = T
  )+

    # Axis limits and titles ----
  ggtitle(print_experiment) +
    xlim(xaxisrange) +
    ylim(c(yaxisrange)) +
    xlab(xlab) +
    ylab("Mean Total Annual Precipitation (mm)") +

    # custom labels and colors for the border ----
  scale_color_manual(name = "Chosen",
                     breaks = c("chosen", "not chosen", "Measured"),
                     values = c("chosen" = 'deepskyblue3',"not chosen" = "grey30","Measured" = "yellow4"))+

    # custom labels and colors for the fill -----
  scale_fill_manual(name = "Chosen",
                    breaks = c("chosen", "not chosen", "Measured"),
                    values = c("chosen" = 'deepskyblue3',"not chosen" = "grey30", "Measured" = "yellow4"))+
    # base theme -----
  theme_bw()+

    # Theme adjustments  -----
  custom_axis_theme

  # remove y axis if not the leftmost graph in the paper  -----
  if(current_experiment != "hist"){
    plot <- plot + theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_blank())
  }

  # save plot  -----
  ggsave(plot = plot, filename = paste0("tpd_",location,"_",current_experiment,"_",print_range,".png"),
         device = "png",
         path = outpath,
         height = 5.33,
         width = 5.33,
         units = "cm"
  )


  #### Extreme Rain -----


  # BIOWATER CHOSEN MODELS/VARIABLES
  chosen_model_runs = c("CNRM-CCLM", "CNRM-RCA", "ECEARTH-RCA", "HADGEM-RCA", "IPSL-RCA")
  xtreme_rain$chosen = if_else(xtreme_rain$MODEL %in% chosen_model_runs, "chosen", "not chosen")
  xtreme_rain$chosen[which(xtreme_rain$MODEL == "Measured")] = "Measured"

  # remove measured for now (the shorter timespan means too little events)
  if(current_experiment != "hist"){xtreme_rain2 <-  xtreme_rain[-11,]}else{xtreme_rain2 <- xtreme_rain}

  title = "Occurances of Extreme Rainfall"
  subtitle = paste0(location, ", ", timerange, ". Scenario: " ,current_experiment)
  yaxis = "Extreme Rainfall Events"
  # the plot
  xtreme_plot <- xtreme_rain2 %>% ggplot(aes(x = reorder(MODEL, get(current_experiment)), y = get(current_experiment),color = "black") )+theme_bw()+
    geom_col(aes(fill = chosen),position = "dodge", width = .7)+
    ylab(yaxis)+
    ylim(c(0,600))+
    scale_fill_manual(name = "",
                      breaks = c("chosen", "not chosen", "Measured"),
                      values = c("chosen" = 'deepskyblue3', "not chosen" = "gray30", "Measured"= "yellow4"))+
    scale_color_manual(values = "black")+


    custom_axis_theme+

    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust =.4, colour = "black", size = 7),
          legend.position = "none",
          axis.title.x = element_blank()
    )


  if(current_experiment != "hist"){
    xtreme_plot <- xtreme_plot + theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_blank())}

  ggsave(
    plot = xtreme_plot,
    filename = paste0("xtreme_rain_",location,"_",current_experiment,"_",print_range,".png"),
    device = "png",
    path = outpath,
    height = 5.33,
    width = 5.33,
    units = "cm"
  )
}

print("plotting finished")

