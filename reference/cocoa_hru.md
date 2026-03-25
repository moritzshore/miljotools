# Generates a connectivity plot for an HRU

There are some important details which need to be observed for using
this function, they are listed below.

## Usage

``` r
cocoa_hru(
  hru_id,
  buildR_dir,
  agri_pattern = "a_",
  directory = NULL,
  save_to_png = FALSE,
  verbose = TRUE
)
```

## Arguments

- hru_id:

  (integer) ID of HRU to plot

- buildR_dir:

  (string) path to SWATbuildR project directory (containing the
  'data/\*' subdirectory)

- agri_pattern:

  (string) a pattern which identifies agriculturial fields (REGEX)

- directory:

  (string) directory where to save plots (optional, only if
  `save_to_png` is enabled. Will be created to sub-directory
  'hru_report/'. If left blank, will use working directory).

- save_to_png:

  (logical) `TRUE` = saves to file, `FALSE` = returns plot

- verbose:

  (logical) Print status?

## Value

Either the path to the plot if `save_to_png` is `TRUE`, or returns the
ggplot object itself if `save_to_png` is `FALSE`

## Details

1.  This function only works with SWATbuildR output, along side with a
    generated `land_connections_as_lines.shp` file which can be
    generated using [this
    code](https://github.com/MR-Eini/Mini_setup_CREATE/blob/main/1_Setup/Libraries/create_connectivity_line_shape.R).

2.  The land use theme is set up for my specific catchment. If you need
    to add to this theme then please send a pull request with the
    additional legend entries.

## See also

[`cocoa_vis()`](https://moritzshore.github.io/miljotools/reference/cocoa_vis.md)
