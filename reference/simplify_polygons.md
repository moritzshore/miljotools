# Simplify Polygons

This function merges flagged polygons into neighboring polygons with the
longest shared border. It will prioritize neighboring polygons of the
same type. See parameter descriptions for more details.

## Usage

``` r
simplify_polygons(polygon_map, type, interactive = FALSE, verbose = TRUE)
```

## Arguments

- polygon_map:

  shapefile containing a column `flag` of type `string` with labels for
  every polygon as either 'flagged' or 'ok'. Flagged polygons will be
  simplified.

- type:

  character string of the name of the `type` column in your shapefile.
  Polygons of the same type will be prioritized for merging If you do
  not need this feature, simply set the type column to your ID column.

- interactive:

  `TRUE/FALSE` Setting this parameter to true will let you inspect the
  changes made.

- verbose:

  print actions to console?

## Value

Returns a modified version of `polygon_map` with the simplifications.

## Examples

``` r
# See Vignette
```
