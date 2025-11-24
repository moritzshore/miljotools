# Aggregate a date range

A wrapper function for
[`metnordic_aggregate()`](https://moritzshore.github.io/miljotools/reference/metnordic_aggregate.md)
which aggregates files from a start to end date (in parallel). You must
supply a variable type and a method of aggregation (eg. mean). This
function is designed to take input from
[`metnordic_download_daterange()`](https://moritzshore.github.io/miljotools/reference/metnordic_download_daterange.md)
and provides input for
[`metnordic_merge_daily()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_daily.md)

## Usage

``` r
metnordic_aggregate_daterange(
  directory,
  variable,
  method,
  start,
  end,
  outpath,
  overwrite = TRUE,
  verify = FALSE,
  n_cores = NULL,
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

  String: MET Nordic variable to aggregate (eg. "precipitation_amount",
  [(see
  more)](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters))

- method:

  String: method of aggregation ("mean", "min", "max", "sum")

- start:

  String: start of the date range to aggregate (eg. "2015-01-01")

- end:

  String: end of the date range to aggregate (eg. "2015-12-31")

- outpath:

  String: path to directory of to be created files

- overwrite:

  Logical: overwrite existing files? (optional, default TRUE)

- verify:

  Logical: verify the existence of all required files? (recommended to
  be TRUE)

- n_cores:

  Numeric: max number of cores to perform operation with. (optional)

- verbose:

  Logical: print to console? (optional)

## Value

Returns a named list with file paths to each aggregated file. `FALSE`
indicates a file failed to be aggregated, likely due to not having 24hrs
(24 individual files) to aggregated from.

## Details

Currently supported are the following types of aggregation: "mean" "min"
"max" "sum". Operations are performed in paralell on multiple cores. You
can control the number of cores used with the `num_cores` parameters.

## See also

[`metnordic_aggregate()`](https://moritzshore.github.io/miljotools/reference/metnordic_aggregate.md)
[`metnordic_download_daterange()`](https://moritzshore.github.io/miljotools/reference/metnordic_download_daterange.md)
[`metnordic_merge_daily()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_daily.md)
