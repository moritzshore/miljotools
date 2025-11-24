# MET Nordic Extract - Point Timeseries regional downloads

This function extracts timeseries from (hourly) files from the chain
[`metnordic_coordwindow()`](https://moritzshore.github.io/miljotools/reference/metnordic_coordwindow.md)
–\>
[`metnordic_buildquery()`](https://moritzshore.github.io/miljotools/reference/metnordic_buildquery.md)
–\>
[`metnordic_download_daterange()`](https://moritzshore.github.io/miljotools/reference/metnordic_download_daterange.md)
–\>
[`metnordic_merge_hourly()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_hourly.md)
–\> `metnordic_extract()`. This function extracts from the nearest grid
cell, if you would like a bi-linear interpolation of the nearest 4
cells, please try
[`metnordic_extract_grid()`](https://moritzshore.github.io/miljotools/reference/metnordic_extract_grid.md)

## Usage

``` r
metnordic_extract(
  directory,
  mn_variables,
  point,
  outdir,
  name,
  verbose = FALSE
)
```

## Arguments

- directory:

  (String) directory containing merged files as created by
  [`metnordic_merge_hourly()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_hourly.md)

- mn_variables:

  (vector, strings) variables to extract

- point:

  an object of class "sf" with point geometry.

- outdir:

  (string) directory in which to write the file (.csv)

- name:

  (string) name of the file (will be added to filename)

- verbose:

  (boolean) print?

## Value

path to written file

## See also

[`metnordic_extract_grid()`](https://moritzshore.github.io/miljotools/reference/metnordic_extract_grid.md)
[`metnordic_merge_hourly()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_hourly.md)
[`swap_metnordic()`](https://moritzshore.github.io/miljotools/reference/swap_metnordic.md)
