# miljotools

This R package hosts a variety of functions for NIBIO DMN Hydrology and Water Environment. If any bugs or problems are encountered, please open a new [issue](https://github.com/moritzshore/miljotools/issues) and we will attempt to resolve it.

## Installing miljotools

You can install and load `miljotools` from the GitHub repository using the following command:

``` r
# install remotes, if not already present
install.packages("remotes")

remotes::install_github(repo = "moritzshore/miljotools", ref = remotes::github_release())

library(miljotools)
```

## MET Nordic Reanalysis Dataset

The MET Nordic Reanalysis Dataset is a reanalysis product from the [Meteorologisk institutt](https://www.met.no/). You can read more about the dataset [here](https://github.com/metno/NWPdocs/wiki/MET-Nordic-dataset). The MET Nordic rerun archive version 3 can be accessed using a dedicated function in `miljotools`. Please inform yourself on the limitations of reanalysis data before applying this dataset to your needs.

![Figure 1: The spatial domain of the reanalysis dataset.](man/figures/domain.png)

### Specifications of the dataset:

-   1x1 km grid covering the Nordics (see Figure 1.)

-   Hourly resolution from 2012.09.01 to 2023.01.31

-   Following variables: temperature, precipitation, relative humidity, wind speed, wind direction, air pressure, cloud area fraction, short+longwave radiation (downwelling), land area fraction, and altitude

### Input

To access the data for a specific region of the Nordics, you need to provide the following as input:

1.  A path to a geo-referenced [shapefile](https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm) (single polygon) of the desired area

2.  A directory where you would like to save the data.

3.  A starting date and time

4.  An ending date and time

Optionally, you can pass additional parameters to either buffer your shapefile, or to preview the data you are downloading.

``` r
download_folder = get_metno_reanalysis3(
  area = "C:/Users/mosh/Documents/skuterud_wb.shp",
  directory = "C:/Users/mosh/Documents/met_no",
  fromdate = "2013-01-01 00:00:00",
  todate = "2022-12-31 00:00:00"
)
```

### Output

![Figure 2: Processing the shapefile and downloading relevant grid points.](man/figures/output.png)

As output you will receive a seperate .csv file for each grid point. This .csv file contains the meteorological data as a timeseries, with a column for each variable. Additionally one metadata file is created listing the properties of each grid point (such as coordinates)

**Note:** Currently, this function must request the data from met.no server on an hour-basis. This means that for each year to download, 8760 requests must be made to the server. This is rather slow, and as such the download can take quite a few hours. We are working with met.no to improve this situation.

### 
