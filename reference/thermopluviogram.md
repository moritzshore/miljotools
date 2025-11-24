# Thermopluviogram

Combines climate data from [Norsk Klima Service
Center](https://klimaservicesenter.no/) and observed data to produce
["Thermopluviograms"](https://www.dwd.de/EN/ourservices/jahresthermo/jahresthermo.html).

## Usage

``` r
thermopluviogram(
  modelled_climate,
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
  experiments = c("hist", "rcp45", "rcp85"),
  left_yaxis_only = FALSE,
  fixed_axis = TRUE,
  axis_font_size = 7,
  yaxisrange = c(800, 1100),
  xaxisrange = c(4.5, 8.5),
  xlab = "Mean Annual Temperature (°C)"
)
```

## Arguments

- modelled_climate:

  filepath of climate files (i.e. "Climate_Data/Modelled/")

- observed_pr:

  filepath of observed precipitation data (.csv)

- observed_tm:

  filepath of the observed mean temperature data (.csv)

- outpath:

  outpath for plotting

- location:

  location of plot (is used for file naming) (string)

- ref_startdate:

  reference period start (ie."1971-01-01")

- ref_enddate:

  reference period d end (ie. "2005-12-31")

- obs_startdate:

  observation period start (ie. "2041-01-01")

- obs_enddate:

  observation period end (ie. "2070-12-31")

- verbose:

  print progress?

- chosen_model_runs:

  Names of the models to color as blue

- experiments:

  Experiments to generate (ie. c("hist", "rcp45","rcp85"))

- left_yaxis_only:

  If set to true, only the historical plot axis will be labeled (This is
  so the three plots can be placed next to each other in a figure)

- fixed_axis:

  use a fixed axis range when plotting? (boolean)

- axis_font_size:

  Font size (Integer)

- yaxisrange:

  y axis range (ie. c(800,1100), mm)

- xaxisrange:

  x axis range (ie. c(4.5, 8.5), Celsius)

- xlab:

  x axis label. (ie. "Mean Annual Temperature)", u00B0 is degree symbol)

## Value

path to generated files

## Details

https://www.senorge.no/PrecTempMap

todo: describe file struture

Generates thermopluviograms and extreme rain index plots from NVE not
flexible. only works with current source data structure.

Date: 01.06.2022 Author: Moritz Shore
