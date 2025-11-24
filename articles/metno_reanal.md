# MET Nordic Reanalysis Dataset

*Author*: Moritz Shore

*Date*: October, 2023

*Last Update*: October, 2025

## Introduction

> Tip: check server status if issues with downloading exist:
> <https://status.met.no/>

The MET Nordic Reanalysis Data set is a reanalysis product from the
[Meteorologisk institutt](https://www.met.no/). You can [read more about
the data set
here](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset). The MET
Nordic rerun archive and operational dataset can be accessed using
dedicated functions in `miljotools`. Please inform yourself on the
benefits and limitations of reanalysis data before applying this data
set to your needs.

![Figure 1: The spatial domain of the reanalysis dataset. (SOURCE: MET
Nordic)](../reference/figures/domain.png)

Figure 1: The spatial domain of the reanalysis dataset. (SOURCE: MET
Nordic)

**Specs**

- **1x1 km** grid covering the Nordics (see Figure 1.)

- **Hourly** resolution from *2012-09-01* to *TODAY*

- **Variables**: temperature, precipitation, relative humidity, wind
  speed, wind direction, air pressure, cloud area fraction,
  short+long-wave radiation (down-welling), land area fraction, and
  altitude.

**Input**

To access the data for a specific region of the Nordics, you need to
provide the following as input:

1.  A path to a georeferenced
    [shapefile](https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm)
    of the desired area

2.  A directory where you would like to save the data (

3.  A starting date and time

4.  An ending date and time

Optionally, you can pass additional parameters to **buffer** your
shapefile, choose your **variables**, choose your **resolution**, or to
preview the data you are downloading.

## Overview

The following figure shows a flowchart of how the different functions
fit together, along with some explanation of the parameters.

![Figure 2: Overview of all the \`metnordic\` functions. You can
download a full version of the image
\[here\](https://gitlab.nibio.no/moritzshore/example-files/-/raw/main/MetNoReanalysisV3/metnordic_functions.png?inline=false)](../reference/figures/metnordic_flowchart.png)

Figure 2: Overview of all the `metnordic` functions. You can download a
full version of the image
[here](https://gitlab.nibio.no/moritzshore/example-files/-/raw/main/MetNoReanalysisV3/metnordic_functions.png?inline=false)

The following will show you how to use these functions, but first the
required libraries need to be loaded for this example workflow:

``` r
require(miljotools)
require(dplyr)
require(sf)
require(stars)
require(mapview)
require(readr)
require(raster)
require(terra)
require(tidyterra)
```

## Coordinate Window

To start, one needs to determine which grid cells of the dataset need to
be downloaded. this can be done with
[`metnordic_coordwindow()`](https://moritzshore.github.io/miljotools/reference/metnordic_coordwindow.html "link to documentation").
This function requires which requires us to pass a geo-referenced
shapefile of our area of interest.

The geometry of the shapefile can be either `polygon` or `point` , and
can either be the `path` to the file (including [sidecar
files](https://desktop.arcgis.com/en/arcmap/latest/manage-data/shapefiles/shapefile-file-extensions.htm))
or an `sf` object in the R environment. Here is an example:

> (By the way, these files are publicly available, so you can try this
> code yourself)

``` r
# downloading example files:
download.file(url = "https://gitlab.nibio.no/moritzshore/example-files/-/raw/main/MetNoReanalysisV3/cs10point.zip", destfile = "cs10point.zip")
download.file(url = "https://gitlab.nibio.no/moritzshore/example-files/-/raw/main/MetNoReanalysisV3/cs10_basin.zip", destfile = "cs10_basin.zip")
unzip("cs10point.zip")
unzip("cs10_basin.zip")
cs10_basin = "cs10_basin/cs10_basin.shp"
cs10_point = "cs10point/cs10point.shp"
example_polygon_geometry <- read_sf(cs10_basin)
example_point_geometry <-  read_sf(cs10_point)
map1 <- mapview(example_polygon_geometry, alpha.regions = .3, legend = F)
map2 <- mapview(example_point_geometry, col.regions = "orange", legend = F)

map1+map2
```

Now, with our geometries loaded, we can create coordinate windows for
them. We will also buffer our polygon shapefile to ensure full coverage.

``` r
coord_window_poly <- metnordic_coordwindow(example_polygon_geometry, area_buffer = 1500)
coord_window_point <- metnordic_coordwindow(example_point_geometry)
```

This gives us a list containing the minimum and maximum x and y cells to
download the data from. `metadist` is NA for polygon geometries.

``` r
paste0(names(coord_window_poly), collapse = ", ")
paste0(coord_window_poly, collapse = ", ")
```

    ## [1] "index_xmin, index_xmax, index_ymin, index_ymax, metadist"
    ## [1] "663, 674, 732, 750, NA"

The `metadist` entry is only needed for point geometry and indicates the
distance to the nearest data set grid cell. Of course, for this geometry
there are only two coordinates:

``` r
paste0(names(coord_window_point), collapse = ", ")
paste0(coord_window_point, collapse = ", ")
```

    ## [1] "index_x, index_y, metadist"
    ## [1] "669, 744, 702"

## Build Query

With our grid-cells set, we can build the queries that we would like to
download from the server. For that we also need to decide the following:

1.  `mn_variables` are the meteorological variables to be downloaded,
    these are [defined
    here](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#parameters "MET No Variables").
2.  `fromdate` and `todate` are time stamps for the period you would
    like to download. Make sure to follow the formatting requirements.
3.  `grid_resolution` is the resolution at which the area will be
    downloaded. For very large regions where a dense network of grid
    cells is not needed, this parameter can be used. A setting of `3`
    for example, will download data from every 3rd cell in the grid, in
    both x and y directions.
4.  `dataset` determines the data source. Classic would be setting this
    to “reanalysis”, which is only the re-run archive, and covers
    approximately 2012-2022. The “operational” data set is also a
    reanalysis product, and covers the time period approximately between
    2018-TODAY. Probably the most useful setting is “continuous” which
    uses the re-run archive where ever possible, and then switches to
    the operational data set once the re-run ends. You can read more
    about this
    [here](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset#available-data-streams "archive versions").

Determining our settings:

``` r
my_variables =c(
  "air_temperature_2m",
  "relative_humidity_2m",
  "wind_speed_10m",
  "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",
  "precipitation_amount"
)
start = "2019-06-01 00:00:00"
end = "2019-06-03 00:00:00"
```

``` r
queries_poly <- metnordic_buildquery(bounding_coords = coord_window_poly,
                                mn_variables = my_variables,
                                fromdate = start, todate = end, 
                                grid_resolution = 1, dataset = "continuous")
```

And for point geometry:

``` r
queries_point <- metnordic_buildquery(bounding_coords = coord_window_point,
                                mn_variables = my_variables,
                                fromdate = start, todate = end, 
                                grid_resolution = 1, dataset = "continuous")
```

We now have the files we want to download:

``` r
queries_point$filenames %>% head()
```

    ## [1] "met_analysis_1_0km_nordic_20190601T00Z.nc"
    ## [2] "met_analysis_1_0km_nordic_20190601T01Z.nc"
    ## [3] "met_analysis_1_0km_nordic_20190601T02Z.nc"
    ## [4] "met_analysis_1_0km_nordic_20190601T03Z.nc"
    ## [5] "met_analysis_1_0km_nordic_20190601T04Z.nc"
    ## [6] "met_analysis_1_0km_nordic_20190601T05Z.nc"

And the `OPenDaP` download urls: (just one as an example, as they are
very long).

``` r
queries_poly$full_urls[1]
```

    ## [1] "https://thredds.met.no/thredds/dodsC/metpparchivev3/2019/06/01/met_analysis_1_0km_nordic_20190601T00Z.nc?x[662:1:673],y[731:1:749],latitude[731:1:749][662:1:673],longitude[731:1:749][662:1:673],altitude[731:1:749][662:1:673],air_temperature_2m[0:1:0][731:1:749][662:1:673],relative_humidity_2m[0:1:0][731:1:749][662:1:673],wind_speed_10m[0:1:0][731:1:749][662:1:673],integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time[0:1:0][731:1:749][662:1:673],precipitation_amount[0:1:0][731:1:749][662:1:673]"

> “metpparchivev3” indicates this comes from the re-run archive. if it
> were to say “metpparchive” only, that would indicate that it comes
> from the operational archive!

## Download

Now we are ready to download the files. Lets download 14:00 as it rained
at that time, a bit:

``` r
poly_path <- "indiv_poly/"
dir.create(poly_path, showWarnings = F)
fps <- metnordic_download(url = queries_poly$full_urls[15],
                          outdir = poly_path,
                          vars = my_variables, verbose = F)
(fps[[1]] %>% terra::rast())[[1]] %>% plot()
```

    ## Warning: [rast] skipped sub-datasets (see 'describe(sds=TRUE)'):
    ## altitude

![](metno_reanal_files/figure-html/unnamed-chunk-12-1.png)

Please note, the downloaded files are separated per variable. Why?
Because the project this code was designed for needed the files like
that. Also, these files get very big very quickly, so having
per-variable separation can sometimes be useful.

### Download a Date Range

Now of course, you would like to download not just one file, but the
whole date range, the following function allows you to do this.

``` r
dl_path = metnordic_download_daterange(
  queries = queries_poly,
  directory = poly_path,
  mn_variables = my_variables)
list.files(dl_path) %>% head()
```

    ## [1] "met_analysis_1_0km_nordic_20190601T00Z_air_temperature_2m.nc"                                            
    ## [2] "met_analysis_1_0km_nordic_20190601T00Z_integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time.nc"
    ## [3] "met_analysis_1_0km_nordic_20190601T00Z_precipitation_amount.nc"                                          
    ## [4] "met_analysis_1_0km_nordic_20190601T00Z_relative_humidity_2m.nc"                                          
    ## [5] "met_analysis_1_0km_nordic_20190601T00Z_wind_speed_10m.nc"                                                
    ## [6] "met_analysis_1_0km_nordic_20190601T01Z_air_temperature_2m.nc"

### Downloading from Point Geometry

Please note!
[`metnordic_download()`](https://moritzshore.github.io/miljotools/reference/metnordic_download.md)
and
[`metnordic_download_daterange()`](https://moritzshore.github.io/miljotools/reference/metnordic_download_daterange.md)
do not work for point geometries. You’re meant to extract from
downloaded .nc files using point locations. But, if you really don’t
want to do that, you can use the
[`metnordic_point()`](https://moritzshore.github.io/miljotools/reference/metnordic_point.md)
function:

``` r
point_path <- "indiv_point/"
dir.create(point_path, showWarnings = F)
point_dl_path <- metnordic_point(
  area = example_point_geometry,
  path = point_path,
  fromdate = start,
  todate = end,
  mn_variables = my_variables,
  verbose = F)
```

    ## Warning in metnordic_point(area = example_point_geometry, path = point_path, : Warning, this function will be retired in the next update, please transition to the new system.
    ##              
    ##  https://moritzshore.github.io/miljotools/articles/metno_reanal.html

``` r
# Viewing the data:
data <- read_csv("indiv_point/METNORDIC_point.csv", show_col_types = F)
plot(data$date, data$air_temperature_2m-273.15, type = "b", 
     ylab = "air_temperature_2m", xlab = "Timestamp", 
     main = "metnordic_point() \ndownload")
```

![](metno_reanal_files/figure-html/unnamed-chunk-14-1.png)

## Merging Hourly Files

If you are working with hourly data, the next logical step would be to
merge the per-hour per-variable files into simply per-variable files.
You can do this like so:

``` r
outpath = "merged_poly/"
dir.create(outpath, showWarnings = F)
custom_merge <- function(variable) {
  metnordic_merge_hourly(folderpath = poly_path,
                         variable = variable,
                         outpath = outpath,
                         verbose = T, overwrite = T)
}
res = lapply(X = my_variables, FUN = custom_merge)
```

    ## miljo🌿tools > metnordic_merge_hourly  >> Merging 49 files:  air_temperature_2m (using 30 threads)
    ## miljo🌿tools > metnordic_merge_hourly  >> Merging 49 files:  relative_humidity_2m (using 30 threads)
    ## miljo🌿tools > metnordic_merge_hourly  >> Merging 49 files:  wind_speed_10m (using 30 threads)
    ## miljo🌿tools > metnordic_merge_hourly  >> Merging 49 files:  integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time (using 30 threads)
    ## miljo🌿tools > metnordic_merge_hourly  >> Merging 49 files:  precipitation_amount (using 30 threads)

``` r
data <- brick(res[[1]])
spatast <- data  %>% rast()
hours = dim(data)[3]
for (i in c(1:hours)) {
  plot(spatast[[i]], legend = FALSE, main = spatast[[i]] %>% names())
}
```

![Hourly Air Temperature in K](metno_reanal_files/figure-html/gif-.gif)

Hourly Air Temperature in K

## Extracting Timeseries

In most cases, one would want a time series from a point on this map,
[`metnordic_extract()`](https://moritzshore.github.io/miljotools/reference/metnordic_extract.md)
does exactly this.

``` r
dir.create("extracted", showWarnings = F)
metnordic_extract(
  directory = "./merged_poly",
  mn_variables = my_variables,
  point = example_point_geometry,
  outdir = "./extracted",
  name = "vignette_example",
  verbose = T)
```

    ## [1] "./extracted/METNORDIC_point_vignette_example.csv"

``` r
# Viewing the data:
data <- read_csv("extracted//METNORDIC_point_vignette_example.csv", show_col_types = F)
plot(data$date, data$air_temperature_2m - 273.15,
  type = "b", ylab = "air_temperature_2m",
  xlab = "Timestamp", main = "metnordic_extract() data")
```

![](metno_reanal_files/figure-html/unnamed-chunk-17-1.png)

If you would like to extract at multiple points, or all points within an
area, you can use
[`metnordic_extract_grid()`](https://moritzshore.github.io/miljotools/reference/metnordic_extract_grid.md)
. This function creates a data file and a metadata file for each point.

``` r
extracted_path = metnordic_extract_grid(
  merged_path =  "merged_poly/",
  area = example_polygon_geometry,
  mn_variables = my_variables,
  outdir = "./extracted_grid",
  verbose = F
)
```

``` r
extracted_path %>% list.files(pattern = "extract_grid_", full.names = T) %>%
  first() %>% read_csv(show_col_types = F) %>% head()
```

    ## # A tibble: 6 × 6
    ##   date                air_temperature_2m relative_humidity_2m wind_speed_10m
    ##   <dttm>                           <dbl>                <dbl>          <dbl>
    ## 1 2019-06-01 00:00:00               282.                0.884           1.30
    ## 2 2019-06-01 01:00:00               282.                0.919           1.42
    ## 3 2019-06-01 02:00:00               282.                0.926           1.33
    ## 4 2019-06-01 03:00:00               282.                0.948           2.07
    ## 5 2019-06-01 04:00:00               282.                0.963           1.72
    ## 6 2019-06-01 05:00:00               282.                0.981           3.17
    ## # ℹ 2 more variables:
    ## #   integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time <dbl>,
    ## #   precipitation_amount <dbl>

## Further processing the data cube

### Aggregating

In some cases you might not want to extract single points and would like
to keep working with the NetCDF4 data cube. If you would like to have
the data in daily form, you can aggregate it (summarize) using
[`metnordic_aggregate()`](https://moritzshore.github.io/miljotools/reference/metnordic_aggregate.md):

``` r
dir.create("aggregated/", showWarnings = F)
agg_path <- metnordic_aggregate(directory = "indiv_poly/", 
                                variable = "air_temperature_2m",
                                method = "mean",
                                day =  "20190601",
                                outpath = "aggregated/",
                                verbose = F, overwrite = T)
```

``` r
data <- brick(agg_path)
spatast <- data  %>% rast()
plot(spatast, main = "mean temp \n(2019-06-01)")
```

![](metno_reanal_files/figure-html/unnamed-chunk-21-1.png)

Now that was only a single day, and usually you would like to aggregate
very many days all at once. For this, you can use the function
[`metnordic_aggregate_daterange()`](https://moritzshore.github.io/miljotools/reference/metnordic_aggregate_daterange.md),
which performs the operation in parallel over multiple cores. If a day
in your date range does not contain 24 individual files (24hrs) it will
not be created and in the returning list will be labelled with `FALSE`.

``` r
agg_dr = metnordic_aggregate_daterange(
  directory =  "indiv_poly/",
  variable = "air_temperature_2m",
  method = "max",
  start = start,
  end = end,
  outpath = "aggregated_range/")
```

### Merging

Now we would usually merge the aggregated days together, with
`miljotools`, like so:

> Tip: don’t forget to add the method of aggregation as a suffix!

``` r
# aggregating a second day to merge it
agg_path <- metnordic_aggregate(directory = "indiv_poly/", 
                                variable = "air_temperature_2m",
                                method = "mean",
                                day =  "20190602",
                                outpath = "aggregated/",
                                verbose = F, overwrite = T)
dir.create("merged_daily/")
merged_fp <- metnordic_merge_daily(folderpath = "aggregated/",
                                   variable = "air_temperature_2m_mean",
                                   outpath = "merged_daily/",overwrite = T)
```

### Re-projecting

If your use-case requires your NetCDF file to be in a specific
projection, you can re-project it like so:

``` r
metnordic_reproject(filepath = merged_fp,
                    outfile = "reprojected_example.nc",
                    projstring = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +type=crs")
```

    ## [1] "reprojected_example.nc"

``` r
reproj <- rast("reprojected_example.nc")
mapview(reproj)
```

## Link to Other Models

So far, conversion of MET Nordic data, as downloaded by `miljotools`,
has been implemented for three models, in order of implementation:
[SWAT+](https://swat.tamu.edu/software/plus/),
[CWatM](https://cwatm.iiasa.ac.at/), and
[SWAP](https://www.swap.alterra.nl/). Here is how it works:

### SWAT+

You can apply MET Nordic data to a SWAT+ setup with the function
[`swatplus_metnordic()`](https://moritzshore.github.io/miljotools/reference/swatplus_metnordic.md)
. This is done with the aid of
[SWATprepR](https://biopsichas.github.io/SWATprepR/). You will need to
use the files written by
[`metnordic_extract_grid()`](https://moritzshore.github.io/miljotools/reference/metnordic_extract_grid.md).

``` r
# downloading an example SWAT+ project
download.file(url = "https://gitlab.nibio.no/moritzshore/example-files/-/raw/main/MetNoReanalysisV3/cs10_txt.zip", destfile = "cs10_txt.zip")
unzip("cs10_txt.zip")

swatplus_metnordic(directory = extracted_path, swat_setup = "cs10_txt/")
```

### CWatM

CWatM takes in NetCDF files, We take the file as projected by
[`metnordic_reproject()`](https://moritzshore.github.io/miljotools/reference/metnordic_reproject.md)
into the correct projection for the setup. Note, this needs to be done
per variable.

``` r
cwatm_convert_nc(infile = "reprojected_example.nc", outfile = "cwatm_ready.nc")
```

    ## [1] "cwatm_ready.nc"

### SWAP

SWAP, being a field scale model, needs point data. We can use the output
of
[`metnordic_extract()`](https://moritzshore.github.io/miljotools/reference/metnordic_extract.md)
in the function
[`swap_metnordic()`](https://moritzshore.github.io/miljotools/reference/swap_metnordic.md)
to create a meteo file for the SWAP model.

``` r
swap_metnordic(
  dldir = "extracted/",
  name = "vignette_example",
  outpath = "metnordic.met",
  timescale = "daily")
```
