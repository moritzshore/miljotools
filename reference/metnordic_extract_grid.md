# Extract data on a (ir)regular grid basis

This function takes the files from
[`metnordic_merge_hourly()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_hourly.md)
and extracts timeseries at desired locations. If a polygon shapefile is
passed to the function, data will be extracted at each grid cells within
the (buffered) area. If a point geometry is supplied, data will be
extracted at each point. A file is written to disk with .csv file for
each grid cell and a metadata file for each grid cell.

## Usage

``` r
metnordic_extract_grid(
  merged_path,
  area,
  buffer = 0,
  mn_variables,
  outdir,
  meta_shp = FALSE,
  verbose = FALSE
)
```

## Arguments

- merged_path:

  String, Path to merged ncdf files from
  [`metnordic_merge_hourly()`](https://moritzshore.github.io/miljotools/reference/metnordic_merge_hourly.md)

- area:

  Geo-referenced shapefile either of polygon or point geometry. Passing
  a polygon geometry will lead to a regular grid being extracted from
  all overlaying points.

- buffer:

  Numeric, buffer in meters. Useful for getting grid cells just outside
  of catchment. Not used if a point geometry is passed.

- mn_variables:

  Character Vector, Met Nordic variables to extract

- outdir:

  String, Folder where data will be written

- meta_shp:

  If set to `TRUE`, then a shapefile of the extracted grid points with
  ID will be written to the `outdir`

- verbose:

  Boolean, print status

## Value

path to written files
