# Create SWAT+ meteo input from MetNo Reanalysis3 data (SUPERSEDED)

**SUPERSEDED**. Note: intended use of this function is within
[`swatplus_metnordic()`](https://moritzshore.github.io/miljotools/reference/swatplus_metnordic.md)
This is now an internal function.

## Usage

``` r
reanalysis3_swatinput(
  path,
  swat_setup,
  write_wgn = TRUE,
  start = NA,
  end = NA,
  sqlite_path = NULL,
  verbose = FALSE,
  backup = FALSE,
  aux_data = NULL,
  epsg_code = NULL,
  fill_missing = NULL,
  clean_files = TRUE
)
```

## Arguments

- path:

  path to daily data provided by `reanalysis3_daily()`

- swat_setup:

  path to your SWAT+ setup. (Required!)

- write_wgn:

  calculate and write the weather generator? defaults to true. (for now
  just based on station \#1 (bottom left))

- start:

  optional parameter to define start date of time series

- end:

  optional parameter to define end date of time series

- sqlite_path:

  path to your SWAT+ sqlite file (only needed if you wish to update your
  database). Warning: start and end parameters will be ignored in this
  case (SWATprepR limitation)

- verbose:

  print status?

- backup:

  (logical, defaults to true) creates a backup of your swat folder
  before modification

## Value

Files are generated in provided paths

## See also

[`swatplus_metnordic()`](https://moritzshore.github.io/miljotools/reference/swatplus_metnordic.md)

Takes data gathered by `get_metno_reanalysis3()` and downscaled by
`reanalysis3_daily()` and creates SWAT+ meteo input files and weather
generators for your SWAT+ input with help from SWATprepR.

**NOTE** Default parameter value for `mn_variables` in
`get_metno_reanalysis3()` is required for this to work.

**NOTE** package `SWATprepR` is required for this function

https://github.com/biopsichas/SWATprepR

## Author

Moritz Shore, Svajunas Plunge
