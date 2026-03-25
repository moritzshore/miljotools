# ERA5 for SWAT Plus

**Author**: Moritz Shore (<moritz.shore@nibio.no>)

**Date**: March, 2026

## ERA5 Meteorological data for SWAT+

To apply ERA5 data to a SWAT+ setup, you need to first independently
download said data from [open-meteo.com](https://open-meteo.com/).
Please note, there is a limit on downloads for the free tier, which is
why (for now) it is not directly implemented in miljotools. When
downloading the data for use in this package, please follow the
following template *EXACTLY*, only changing the grid cells (list
format), start and end date, and perhaps the variables (*UNTESTED – make
sure to adjust the `open_meteo_variables`* parameter). Any deviations
will probably cause errors. If you would like to make the function more
robust for your use case, feel free to submit a pull request!

Here is the template
[URL](https://open-meteo.com/en/docs/historical-weather-api?timezone=Europe%2FBerlin&models=era5&hourly=&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,shortwave_radiation_sum,wind_speed_10m_mean,relative_humidity_2m_mean&wind_speed_unit=ms&bounding_box=59.522321,10.82686,59.865845,11.111253&start_date=2013-01-01&end_date=2022-12-31&location_mode=csv_coordinates&cell_selection=nearest&csv_coordinates=59.75,+10.75%0A59.75,11.00%0A59.50,10.75%0A59.50,11.00%0A#hourly_weather_variables):

    https://open-meteo.com/en/docs/historical-weather-api?timezone=Europe%2FBerlin&models=era5&hourly=&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,shortwave_radiation_sum,wind_speed_10m_mean,relative_humidity_2m_mean&wind_speed_unit=ms&bounding_box=59.522321,10.82686,59.865845,11.111253&start_date=2013-01-01&end_date=2022-12-31&location_mode=csv_coordinates&cell_selection=nearest&csv_coordinates=59.75,+10.75%0A59.75,11.00%0A59.50,10.75%0A59.50,11.00%0A#hourly_weather_variables

Once you have downloaded your data, You can use the following function
to apply ERA5 meteorological data to a SWAT+ setup:

``` r
require(miljotools)
era5_swatplus(open_meteo_path = "path/to/file.csv",
              extract_path = "path/to/directory",
              swat_setup = "path/to/directory")
```

Please read up on the optional parameters on the help page! This
function uses [SWATprepR](https://biopsichas.github.io/SWATprepR/) to
apply the meteo data.
