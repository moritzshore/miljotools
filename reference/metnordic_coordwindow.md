# Get a MET Nordic Coordinate Window

This function retrieves the coordinate window from a shapefile for
downloading MET Nordic files. This window can then be passed to
[`metnordic_buildquery()`](https://moritzshore.github.io/miljotools/reference/metnordic_buildquery.md)
to parse OPENDAP urls to download. You can also set `source` to
"senorge" to get the coordinate window for the SeNorge2018 grid.

## Usage

``` r
metnordic_coordwindow(
  area_path,
  area_buffer = 0,
  source = "metnordic",
  verbose = FALSE,
  interactive = FALSE
)
```

## Arguments

- area_path:

  String: path to shapefile of region / point (this must have point or
  polygon geometry!)

- area_buffer:

  Integer: buffer in m to place around shapefile / point

- source:

  String: 'metnordic' (default) to access the MET Nordic grid, or
  'senorge' to access the SeNorge2018 grid.

- verbose:

  Logical: plot the coordinate window?

- interactive:

  Logical, should the plotted coordinate window be interactive (mapview)
  or static (ggplot)

## Value

returns a list of the min and max x and y cells for downloading.

## See also

[`metnordic_buildquery()`](https://moritzshore.github.io/miljotools/reference/metnordic_buildquery.md)

## Author

Moritz Shore

## Examples

``` r
# TODO
```
