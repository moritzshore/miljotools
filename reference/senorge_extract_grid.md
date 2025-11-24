# Extract data from SeNorge2018 files

This function extracts data from SeNorge2018 .nc files from overlapping
grid cells of a provided polygon.

## Usage

``` r
senorge_extract_grid(
  directory,
  outdir,
  area,
  variables,
  buffer = 0,
  verbose = FALSE,
  map = TRUE
)
```

## Arguments

- directory:

  string, path to directory of downloaded SeNorge2018 files (as
  downloaded by
  [`senorge_download()`](https://moritzshore.github.io/miljotools/reference/senorge_download.md))

- outdir:

  string, path to directory where data should be extracted to.

- area:

  Shapefile of polygon geometry of which region should be extracted.

- variables:

  character vector of SeNorge2018 variables to be extracted ("tg", "tn",
  "tx", "rr")

- buffer:

  integer, buffer (in meters) that should be applied to the provided
  shapefile for data extraction

- verbose:

  logical, text be printed to console?

- map:

  logical, should maps be printed?

## Value

Returns path to extracted files.

## See also

[`senorge_download()`](https://moritzshore.github.io/miljotools/reference/senorge_download.md)

## Examples

``` r
# TODO.
```
