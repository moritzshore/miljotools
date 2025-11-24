# Download MET Nordic data for one point

note: this function only works for point geometry in csv format.

## Usage

``` r
metnordic_point(
  area,
  path = NULL,
  fromdate = "2012-09-01 10:00:00",
  todate = "2012-09-01 20:00:00",
  mn_variables = NULL,
  dataset = "continuous",
  verbose = TRUE
)
```

## Arguments

- area:

  (string) path to geo-referenced shapefile (point) of the desired area.
  (optionally, you can pass a `sf` object directly.)

- path:

  (string) path to download directory

- fromdate:

  (string) date and time for start of time series (ie. "2012-09-01
  10:00:00")

- todate:

  (string) date and time for end of time series (ie. "2013-09-01
  10:00:00")

- mn_variables:

  (vector) Leave blank for default variables

- dataset:

  (string) from which dataset to source the files from? ("reanalysis",
  "operational", or "continuous" for both).

- verbose:

  (boolean) generate graphs showing previews of data download?

## Value

Function returns a path to where .csv files of the download were
written. One .csv file for each grid point within the (buffered) shape
file area. Additionally one metadata file (.csv) is written with the

## Details

This is a modified version of the legacy `get_metno_reanalysis3`
designed specifically for downloading single points in csv format.

## Author

Moritz Shore

## Examples

``` r
 if(FALSE){
 metnordic_csv(
 area = example_file_path,
 fromdate = "2015-01-01",
 todate = "2015-01-02",
 verbose = TRUE
 )
 }

```
