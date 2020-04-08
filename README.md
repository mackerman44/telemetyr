# telemetyr

`telemetyr` is an R package to ease the burden of processing and interpreting oodles of telemetry observation data. More specifically, `telemetyr` aims to provide tools to ease the reading, cleaning, reduction, analysis, and visualization of telemetry observational data.

Title: Tools for processing, analysis, and visualization of telemetry data
Description: An R package for reading, cleaning, reducing, analysis, and 
    visualization of telemetry observation data. The package is being 
    initially build using radio telemetry data from a project to characterize
    the distribution, movement, and survival of juvenile Chinook salmon in 
    the Lemhi and Salmon rivers, Idaho, during winter months. Tools 
    include the cleaning and reduction of data to prepare capture histories 
    for a Cormack-Jolly-Seber model and also to evaluate the habitat 
    availability and use to describe preferred or target habitat conditions. 
    Our hope is to build the package with functions and tools that can be 
    applied elsewhere to telemetry or similar observational datasets.

`SCOBI` is an R package for performing compositional analyses of adults and smolts at Lower Granite Dam. Adults are analyzed using 
the function `SCOBI()`. Juveniles are analyzed using the function `SCRAPI()`. It also contains the useful functions `lgr2SCOBI()` 
and `lgr2SCRAPI()` for formatting raw data from the Lower Granite Dam trapping database (LGTrappingDB) for input into the `SCOBI()`
and `SCRAPI()` functions.

## Getting Started

To install `SCOBI` you can use Hadley Wickham's `devtools` package. To install and load the `devtools` package use:
```
install.packages("devtools")
library(devtools)
```
NOTE: To use `devtools`, you may also have to download and install Rtools (although you shouldn't). The latest version on Rtools can be found at
https://cran.r-project.org/bin/windows/Rtools/

Once `devtools` is successfully installed, use the following to install SCOBI:
```
devtools::install_github("mackerman44/SCOBI")
```
Alternatively, Bryce Oldemeyer has a GitHub account and maintains an up-to-date fork of the SCOBI repository. Therefore, SCOBI can
also be downloaded using:
```
devtools::install_github("Boldemeyer/SCOBI")
```
If you are interested in making contributions to SCOBI, consider getting a GitHub account, fork this repository, clone to a local directory, modify, and send me a pull request. I can then review any changes and merge.

For further information, see:

Steinhorst, K. T. Copeland, M. W. Ackerman, W. C. Schrader, and E. C. Anderson. (*In review*) Estimates and Confidence Intervals
for Run Composition of Returning Salmonids. Fishery Bulletin.

Enjoy!
