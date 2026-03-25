# Visualize COCOA on a catchment scale

NOTE: this function has a few requirements to work, please see below.

## Usage

``` r
cocoa_vis(buildR_dir, directory, RERENDER = TRUE, verbose = TRUE)
```

## Arguments

- buildR_dir:

  (string) path to SWATbuildR project directory (containing the
  'data/\*' subdirectory)

- directory:

  (string) directory where to save/load the generated HRU plots to/from.
  (will be saved in a sub-directory 'hru_report/')

- RERENDER:

  (logical) re-generate the HRU plots?

- verbose:

  (logical) print status?

## Value

`mapview` map of HRUs in the catchment. Click to view each HRUs
connectivity.

## Details

1.  This function only works with SWATbuildR output, along side with a
    generated `land_connections_as_lines.shp` file which can be
    generated using [this
    code](https://github.com/MR-Eini/Mini_setup_CREATE/blob/main/1_Setup/Libraries/create_connectivity_line_shape.R).

2.  The land use theme is set up for my specific catchment. If you need
    to add to this theme then please send a pull request with the
    additional legend entries.

## See also

[`cocoa_hru()`](https://moritzshore.github.io/miljotools/reference/cocoa_hru.md)
