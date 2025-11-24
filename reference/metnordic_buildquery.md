# Build MET Nordic Download Query

This function builds the URL queries for downloading MET Nordic data
through the OPENDAP protocol. The requirements for this function to work
are the bounding coordinates as divined by function
[`metnordic_coordwindow()`](https://moritzshore.github.io/miljotools/reference/metnordic_coordwindow.md),
the variables of interest, the starting and ending dates and the desired
grid resolution. The results of this function can be downloaded when
passed to
[`metnordic_download()`](https://moritzshore.github.io/miljotools/reference/metnordic_download.md).

## Usage

``` r
metnordic_buildquery(
  bounding_coords,
  mn_variables,
  fromdate,
  todate,
  grid_resolution = 1,
  dataset = "reanalysis",
  verbose = FALSE
)
```

## Arguments

- bounding_coords:

  as passed by
  [`metnordic_coordwindow()`](https://moritzshore.github.io/miljotools/reference/metnordic_coordwindow.md)

- mn_variables:

  MET Nordic variables (see
  [documentation](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters))

- fromdate:

  ie. "2019-01-01 00:00:00"

- todate:

  ie. "2020-12-31 23:00:00"

- grid_resolution:

  an integer, ie. 3 for 3x3 km grid.

- dataset:

  either 'reanalysis' for the re-run archive, 'operational' for the
  operational archive, 'ltc' for the EXPERIMENTAL long term consistent
  product or 'continuous' to source from all, depending on date range.

- verbose:

  print to console?

## Value

character vector of all the OPENDAP URLs to download.

## See also

[`metnordic_coordwindow()`](https://moritzshore.github.io/miljotools/reference/metnordic_coordwindow.md)
[`metnordic_download()`](https://moritzshore.github.io/miljotools/reference/metnordic_download.md)
[`metnordic_download_daterange()`](https://moritzshore.github.io/miljotools/reference/metnordic_download_daterange.md)

## Author

Moritz Shore

## Examples

``` r
# TODO
```
