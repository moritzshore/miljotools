# Download MET Nordic files

Downloads a list of provided queries from the MET No thredds server
using the OPENDAP protocol. The queries should be passed from
`metnordic_buildqueries()`. Folder of where to download, and which
variables to download also need to be provided! Any files already
present in the folder will not be re-downloaded. This means that if the
download fails for whatever network reason, you can just restart the
function and it should pick off where it left off. If you are having
issues with downloads, make sure to check https://status.met.no/ for
server (THREDDS) status.

## Usage

``` r
metnordic_download_daterange(queries, directory, mn_variables, verbose = FALSE)
```

## Arguments

- queries:

  list as passed by `metnordic_buildqueries()`

- directory:

  folder where to download

- mn_variables:

  MET Nordic variables to download.

- verbose:

  print status?

## Value

path to download directory

## See also

[`metnordic_buildquery()`](https://moritzshore.github.io/miljotools/reference/metnordic_buildquery.md)
[`metnordic_merge_hourly()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_hourly.md)
[`metnordic_aggregate()`](https://moritzshore.github.io/miljotools/reference/metnordic_aggregate.md)
