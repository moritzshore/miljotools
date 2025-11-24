# Verify MET Nordic download

This function scans the download folder for files that are "too small"
and then deletes them. You can then use the
`download_metnordic_daterange()` with the same directory (and queries
from `metnordic_build_query()`) function to re-download the deleted ones

## Usage

``` r
metnordic_verify_download(folder)
```

## Arguments

- folder:

  path of downloaded files

## Value

Nothing

## Examples

``` r
# TODO
```
