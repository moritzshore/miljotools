# miljotools <img src="man/figures/logo.png" align="right" height="138"/>

This R package aims to host a variety of functions useful for data collection, processing, and analysis in environmental modelling in the context of NIBIO's "[Environmental Modelling and Measures](https://www.nibio.no/en/subjects/environment/environmental-modelling-and-measures)" group. For more information, please see the "[Support Tools](https://www.nibio.no/en/subjects/environment/environmental-modelling-and-measures/support-tools)" page. If any bugs or problems are encountered, please open a new [issue](https://github.com/moritzshore/miljotools/issues) and we will attempt to resolve it.

![GitHub R package version](https://img.shields.io/github/r-package/v/moritzshore/miljotools)
[![R-CMD-check](https://github.com/moritzshore/miljotools/actions/workflows/rcmdcheck.yml/badge.svg)](https://github.com/moritzshore/miljotools/actions/workflows/rcmdcheck.yml) 
![GitHub repo size](https://img.shields.io/github/repo-size/moritzshore/miljotools)
![GitHub License](https://img.shields.io/github/license/moritzshore/miljotools)
![GitHub top language](https://img.shields.io/github/languages/top/moritzshore/miljotools)

**Table of contents**

1. [Introduction](#introduction)

2.  [Installing miljotools](#install)

3.  [Tools](#start)

4.  [Acknowledgements](#ack)

## Introduction <a name="introduction"></a>

A large fraction of the workload for a “modeler” in environmental sciences consists of “workflows”. These workflows typically revolve around getting data, converting data, processing data before actually using the data in a model. Even once used, data needs to be post-processed before it can bring meaningful insight. As such, coding, scripting, automation “tools” are used by many people. However, the use of these tools is often restricted to either the author, or the small network of close colleagues, even if they could be useful to a wide variety of researchers. Miljotools aims to partially remedy this fragmentation of useful “tools” by providing a repository of generalized and documented functions, workflows, scripts, etc. These are provided in an open-source R package “miljotools”

Note: This unofficial side project is still in its infancy, and currently does not encompass much functionality. If you would like to contribute, please reach out to the maintainer for developer access.

## Installing `miljotools` <a name="install"></a>

You can install and use `miljotools` in `R`, downloaded from the GitHub repository using the following command:

``` r
# install remotes, if not already present
install.packages("remotes")

remotes::install_github(repo = "moritzshore/miljotools", ref = remotes::github_release())

library(miljotools)
```

## Tools <a name="start"></a>

View the "Articles" to get started

1.  [MET Nordic Reanalysis Dataset](https://moritzshore.github.io/miljotools/articles/metno_reanal.html)

2.  [Soil Classification](https://moritzshore.github.io/miljotools/articles/Norwegian_Soil_Classification.html)

3.  [Thermopluviograms](https://moritzshore.github.io/miljotools/articles/thermopluviograms.html)
 
4.  More to come...

## Add your own <a name="add"></a>

If you have a function / code / workflow / script / etc. which you would like to add to this package, please contact the maintainer for developer access.

## Acknowledgements <a name="ack"></a>

The development of this package was supported by the [NIBIO Environmental Modelling and Measures group](https://www.nibio.no/en/subjects/environment/environmental-modelling-and-measures?locationfilter=true), under the [Support Tools](https://www.nibio.no/en/subjects/environment/environmental-modelling-and-measures/support-tools?locationfilter=true) designation.

Additionally, this package was in part developed for the [OPTAIN](https://optain.eu/) project and has received funding from the European Union's Horizon 2020 research and innovation program under grant agreement No. 862756.

Finally, the logo is heavily inspired (celebrating) the old <img src="https://github.com/user-attachments/assets/cc004282-b64d-4357-8350-a3a48f279053" width="70" title="BioForsk"/> logo

![](https://github.com/user-attachments/assets/e05d7984-a936-4114-8fc9-954444589a67)

