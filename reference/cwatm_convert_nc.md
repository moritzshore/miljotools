# Convert MET Nordic Files to CWatM Meteo Input

This function converts a projected MET Nordic file (from
[`metnordic_reproject()`](https://moritzshore.github.io/miljotools/reference/metnordic_reproject.md))
to be compatible with CWatm. This function mainly just changes the
dimension names from easting and northing to x and y and swaps variable
order but has a crap load of boiler plate because... well.. ncdf4.
Supported variables are: tmax, tmin, prec, RH, shortwave downwelling,
air pressure, wind speed

## Usage

``` r
cwatm_convert_nc(
  infile,
  outfile,
  altitude = NULL,
  temperature = NULL,
  verbose = FALSE
)
```

## Arguments

- infile:

  String: path to projected MET Nordic .nc file as created by
  [`metnordic_reproject()`](https://moritzshore.github.io/miljotools/reference/metnordic_reproject.md)

- outfile:

  String: path and file name to desired output file

- altitude:

  Only needed for converting to surface pressure. Path to file which
  contains altitude for the grid (.nc, ie as downloaded by
  [`metnordic_download()`](https://moritzshore.github.io/miljotools/reference/metnordic_download.md))

- temperature:

  Only needed for converting to surface pressure. Path to file which
  contains temperature data for a data cube of the same dimension.

- verbose:

  Logical: print to console?

## Value

Path to outfile

## Details

Code largely adapted from this handy guide:
([link](https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-projected-netcdf-file))
from Pat Bartlein, bartlein@uoregon.edu

## See also

[`metnordic_reproject()`](https://moritzshore.github.io/miljotools/reference/metnordic_reproject.md)

## Author

Moritz Shore

## Examples

``` r
#TODO
```
