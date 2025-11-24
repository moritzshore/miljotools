# Convert hourly MET Nordic files to daily

This function takes 24 hourly files from the same day and variable and
converts them to a daily format. You have the choice of four different
aggregation methods: min, max, mean, and sum.

## Usage

``` r
metnordic_aggregate(
  directory,
  variable,
  method,
  day,
  outpath,
  overwrite = TRUE,
  verbose = TRUE
)
```

## Arguments

- directory:

  String: Path to the source files (as downloaded by
  [`metnordic_download()`](https://moritzshore.github.io/miljotools/reference/metnordic_download.md)
  or
  [`metnordic_download_daterange()`](https://moritzshore.github.io/miljotools/reference/metnordic_download_daterange.md))

- variable:

  String: MET Nordic variable to aggregate (ie. "precipitation_amount",
  [(see
  more)](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters))

- method:

  String: method of aggregation ("mean", "min", "max", "sum")

- day:

  String: day to convert ("YYYYMMDD" format, ie. "20150901")

- outpath:

  String: path to directory of to be created files

- overwrite:

  Logical: overwrite existing file?

- verbose:

  Logical: plot map?

## Value

String: path to written file

## Details

This function is designed to take input from
[`metnordic_download()`](https://moritzshore.github.io/miljotools/reference/metnordic_download.md)
and provides input for `metnordic_merge()`

Code largely adapted from this handy guide:
([link](https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-projected-netcdf-file))
from Pat Bartlein, bartlein@uoregon.edu

## See also

[`metnordic_merge_daily()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_daily.md)
[`metnordic_download()`](https://moritzshore.github.io/miljotools/reference/metnordic_download.md)
[`metnordic_download_daterange()`](https://moritzshore.github.io/miljotools/reference/metnordic_download_daterange.md)

## Author

Moritz Shore

## Examples

``` r
# TODO
```
