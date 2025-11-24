# Apply MetNordic meteo data to a SWAT+ setup

This function takes the output from
[`metnordic_extract_grid()`](https://moritzshore.github.io/miljotools/reference/metnordic_extract_grid.md)
and applies it to a SWAT+ setup, with the help of
[SWATprepR](https://biopsichas.github.io/SWATprepR/).

## Usage

``` r
swatplus_metnordic(
  directory,
  swat_setup,
  write_wgn = TRUE,
  start = NA,
  end = NA,
  sqlite_path = NULL,
  backup = FALSE,
  aux_data = NULL,
  epsg_code = NULL,
  fill_missing = TRUE,
  clean_files = TRUE,
  verbose = FALSE
)
```

## Arguments

- directory:

  path to weather data as created by
  [`metnordic_extract_grid()`](https://moritzshore.github.io/miljotools/reference/metnordic_extract_grid.md)

- swat_setup:

  path to SWAT+ setup

- write_wgn:

  should the weather generator be written?

- start:

  optional parameter to define start date of time series

- end:

  optional parameter to define end date of time series4

- sqlite_path:

  path to your SWAT+ sqlite file (only needed if you wish to update your
  database). Warning: start and end parameters will be ignored in this
  case (SWATprepR limitation)

- backup:

  (logical, defaults to true) creates a backup of your swat folder
  before modification

- aux_data:

  Additional (non-metnordic) data to add to your SWAT+ project. Must be
  a path to an .xlsx file which matches the `SWATprepR` format of
  `load_template()`.

- epsg_code:

  `SWATprepR`: (optional) Integer, EPSG code for station coordinates.
  Default epsg_code = 4326, which stands for WGS 84 coordinate system.

- fill_missing:

  `SWATprepR`: (optional) Boolean, TRUE - fill data for missing stations
  with data from closest stations with available data. FALSE - leave
  stations without data. Weather generator will be used to fill missing
  variables for a model. Default fill_missing = TRUE.

- clean_files:

  `SWATprepR`: Logical, if `TRUE`, will remove all existing weather
  files in model setup folder before writing new ones. Default
  `clean_files = TRUE`.

- verbose:

  print to console?

## Value

path to SWAT+ setup

## See also

[`metnordic_extract_grid()`](https://moritzshore.github.io/miljotools/reference/metnordic_extract_grid.md)
