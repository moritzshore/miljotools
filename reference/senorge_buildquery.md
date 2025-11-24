# Build SeNorge2018 Nordic Download Query

This function builds the URL queries for downloading SeNorge data
through the OPENDAP protocol. The requirements for this function to work
are the bounding coordinates as divined by function
[`metnordic_coordwindow()`](https://moritzshore.github.io/miljotools/reference/metnordic_coordwindow.md),
the variables of interest, the starting and ending dates and the desired
grid resolution. The results of this function can be downloaded when
passed to
[`senorge_download()`](https://moritzshore.github.io/miljotools/reference/senorge_download.md).

## Usage

``` r
senorge_buildquery(
  bounding_coords,
  variables,
  fromdate,
  todate,
  grid_resolution = 1,
  verbose = FALSE
)
```

## Arguments

- bounding_coords:

  list as returned by
  [`metnordic_coordwindow()`](https://moritzshore.github.io/miljotools/reference/metnordic_coordwindow.md)

- variables:

  character vector of variables to download may include the following:
  "tn", "tx", "rr", "tg"

- fromdate:

  date from when to start downloading data from (eg. "1957-01-01")

- todate:

  date from when to end downloading data from (eg. "2024-12-31")

- grid_resolution:

  integer value fo grid resolution (eg 1 for a 1x1km grid)

- verbose:

  logical, print?

## Value

Returns a named list with the urls and filenames which is to be passed
to
[`senorge_download()`](https://moritzshore.github.io/miljotools/reference/senorge_download.md).

## See also

[`metnordic_coordwindow()`](https://moritzshore.github.io/miljotools/reference/metnordic_coordwindow.md)
[`senorge_download()`](https://moritzshore.github.io/miljotools/reference/senorge_download.md)

## Examples

``` r
# TODO
```
