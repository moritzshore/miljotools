# Convert ERA5 data from open meteo to SWAT+ input.

Usage of this function require the user to first download the ERA5 data
from open-meteo. Please see below for instructions.

## Usage

``` r
era5_swatplus(
  open_meteo_path,
  open_meteo_variables = c("temperature_2m_max", "temperature_2m_min",
    "precipitation_sum", "shortwave_radiation_sum", "wind_speed_10m_mean",
    "relative_humidity_2m_mean"),
  extract_path,
  swat_setup,
  aux_data = NULL,
  epsg_code = NULL,
  verbose = TRUE,
  selected_vars = c("max_temp", "min_temp", "precipitation_amount", "rad", "windspeed",
    "rh"),
  write_wgn = TRUE,
  clean_files = TRUE,
  sqlite_path = NULL,
  fill_missing = FALSE
)
```

## Arguments

- open_meteo_path:

  path to file downloaded from open-meteo in daily format with the
  correct variables

- open_meteo_variables:

  character vector of which variables were downloaded. IMPORT TO KEEP
  THE ORDER CORRECT (same as in API request!). The default values are
  those needed for the SWAT+ setup.

- extract_path:

  path to location where data can be extracted and processed

- swat_setup:

  path to swat+ setup

- aux_data:

  if auxiliary data is to be added, path to xlsx file using `SWATprepR`
  formatting.

- epsg_code:

  See `SWATprepR` docs

- verbose:

  Print? default is `TRUE`

- selected_vars:

  Variables to incorporate from ERA5. default is all of them.

- write_wgn:

  See `SWATprepR` docs

- clean_files:

  See `SWATprepR` docs

- sqlite_path:

  See `SWATprepR` docs

- fill_missing:

  See `SWATprepR` docs

## Value

Nothing

## Details

To apply ERA5 data to a SWAT+ setup, you need to first independently
download said data from open-meteo.com. Please note, there is a limit on
downloads for the free tier, which is why (for now) it is not directly
implemented in miljotools. When downloading the data for use in this
package, please follow the following template EXACTLY, only changing the
grid cells (list format), start and end date, and perhaps the variables
(UNTESTED – make sure to adjust the open_meteo_variables parameter). Any
deviations will probably cause errors. If you would like to make the
function more robust for your use case, feel free to submit a pull
request!

Here is the template URL:
\[[link](https://open-meteo.com/en/docs/historical-weather-api?timezone=Europe%2FBerlin&models=era5&hourly=&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,shortwave_radiation_sum,wind_speed_10m_mean,relative_humidity_2m_mean&wind_speed_unit=ms&bounding_box=59.522321,10.82686,59.865845,11.111253&start_date=2013-01-01&end_date=2022-12-31&location_mode=csv_coordinates&cell_selection=nearest&csv_coordinates=59.75,+10.75%0A59.75,11.00%0A59.50,10.75%0A59.50,11.00%0A#hourly_weather_variables)\]
Once you have downloaded your data, You can use the function to apply
ERA5 meteorological data to a SWAT+ setup:

Please see the [SWATprepR docs](https://biopsichas.github.io/SWATprepR/)
for details on some of the optional parameters.
