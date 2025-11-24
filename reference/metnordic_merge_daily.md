# Merge MET Nordic Files (daily)

This function merges **daily** MET Nordic files into a single .nc file
for a single variable. The input data should be the output data of
[`metnordic_aggregate()`](https://moritzshore.github.io/miljotools/reference/metnordic_aggregate.md).
This function provides input for
[`metnordic_reproject()`](https://moritzshore.github.io/miljotools/reference/metnordic_reproject.md).
For **hourly** merging, please use
[`metnordic_merge_hourly()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_hourly.md)

## Usage

``` r
metnordic_merge_daily(
  folderpath,
  variable,
  outpath,
  overwrite = FALSE,
  verify = FALSE
)
```

## Arguments

- folderpath:

  String: Outpath of files from
  [`metnordic_aggregate()`](https://moritzshore.github.io/miljotools/reference/metnordic_aggregate.md).

- variable:

  String: [MET Nordic
  variable](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters)
  to be used (**NOTE:** don't forget suffix such as 'mean', 'sum',
  'min', or 'max').

- outpath:

  String: Folder path where to write files.

- overwrite:

  Logical: Overwrite existing file?

- verify:

  Logical: check if all dates in date range exist?

## Value

file path of generated files

## Details

Code largely adapted from this handy guide:
([link](https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-projected-netcdf-file))
from Pat Bartlein, bartlein@uoregon.edu

## See also

[`metnordic_aggregate()`](https://moritzshore.github.io/miljotools/reference/metnordic_aggregate.md)
[`metnordic_reproject()`](https://moritzshore.github.io/miljotools/reference/metnordic_reproject.md)
[`metnordic_merge_hourly()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_hourly.md)

## Author

Moritz Shore

## Examples

``` r
# TODO
```
