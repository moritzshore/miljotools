# Download a MET Nordic Data URL

Downloads MET Nordic re-analysis data using an OPENDAP url. This
function ideally takes input from
[`metnordic_buildquery()`](https://moritzshore.github.io/miljotools/reference/metnordic_buildquery.md)
and provides input for
[`metnordic_aggregate()`](https://moritzshore.github.io/miljotools/reference/metnordic_aggregate.md).
If you are having issues with downloads, make sure to check
https://status.met.no/ for server (THREDDS) status.

## Usage

``` r
metnordic_download(url, outdir, vars, overwrite = FALSE, verbose = TRUE)
```

## Arguments

- url:

  String: OPENDAP url to be downloaded (from \`metnordic_buildquery()).

- outdir:

  String: Location where the .nc file should be written.

- vars:

  Vector: MET Nordic Variables to extract. See: [MET Nordic
  variables](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters)

- overwrite:

  Logical: Overwrite files if they already exist?

- verbose:

  Logical: Print preview?

## Value

String: file paths to generated files

## Details

Code largely adapted from this handy guide:
([link](https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-projected-netcdf-file))
from Pat Bartlein, bartlein@uoregon.edu

## See also

[`metnordic_buildquery()`](https://moritzshore.github.io/miljotools/reference/metnordic_buildquery.md)
[`metnordic_aggregate()`](https://moritzshore.github.io/miljotools/reference/metnordic_aggregate.md)
[`metnordic_download_daterange()`](https://moritzshore.github.io/miljotools/reference/metnordic_download_daterange.md)
[metnordic_merge_hourly](https://moritzshore.github.io/miljotools/reference/metnordic_merge_hourly.md)

## Author

Moritz Shore

## Examples

``` r
#TODO
```
