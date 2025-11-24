# Re-project MET Nordic Data

Takes in MET Nordic .nc files as created by `metnordic_merge()` and
re-projects them to desired projection.

## Usage

``` r
metnordic_reproject(filepath, outfile, projstring = NULL)
```

## Arguments

- filepath:

  path to .nc file to be reprojected

- outfile:

  filepath of .nc file to be created. (if the same file path is used,
  .nc file will be overwritten)

- projstring:

  desired projection in [proj4 format](https://epsg.io/docs). By
  default, [UTM33N](https://epsg.io/32633) will be used.

## Value

Filepath to re-projected file

## Details

**NOTE:** currently only the following projstring has been tested:

`projstring <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +type=crs"`

## See also

[`metnordic_merge_daily()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_daily.md)
[`cwatm_convert_nc()`](https://moritzshore.github.io/miljotools/reference/cwatm_convert_nc.md)

## Author

Moritz Shore

## Examples

``` r
# TODO
```
