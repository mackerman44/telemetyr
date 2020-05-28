# telemetyr

`telemetyr` is an R package to ease the burden of processing and interpreting oodles of telemetry observation data. More specifically, `telemetyr` aims to provide tools to simplify reading, cleaning, reduction, analysis, and visualization of telemetry observational data. The package is initially being developed as part of a radio telemetry project to characterize the distribution, movement, and survival of juvenile Chinook salmon emigrants in the Lemhi and Salmon rivers, Idaho, USA, during winter months. Currently, the package is written specific to radio telemetry data downloaded from NOAA receivers and using the Tracker software; however, the long-term goal is that functions and tools within `telemetyr` be developed further to accommodate any type of telemetry data (e.g. acoustic, radio, etc.) including from any type of receiver make or model. 

At the writing of this `README` file, the package includes functionality to import, clean, and compress telemetry observations in preparation for analysis. Cleaned observations can then easily be turned into long- or wide-form capture histories which can then be used for data analysis and visualization. Some functions and documentation are also included to help the user run a Cormack-Jolly-Seber model in a Bayesian framework. We also include functions that can be used for perform simple telemetry site "diagnostics" including evaluations of site operation times, site "noise", and the like. Finally, functions are available to summarise and visualize movement, spatially and temporally, along a river network.

The authors hope to continue to develop `telemetyr` in the long-term and so any feedback on bugs, additional desired functionality, or similar are always appreciated. To report a bug, request features, report errors in documentation, etc. please visit the *Issues* page for the `telemetyr` package [here](https://github.com/mackerman44/telemetyr/issues/) and start a "New issue".

## Getting Started

To install the current working version of this package to your computer, you can use Hadley Wickham's `devtools` package. To install and load `devtools` (or any package for that matter) use:
```
install.packages("devtools")
library(devtools)
```

Once `devtools` is successfully installed, use the following to install `telemetyr`:
```
devtools::install_github("mackerman44/telemetyr", build_vignettes = TRUE)
```

Be sure to include the `build_vignettes = TRUE` argument, as this will ensure that vignettes included with the package will be built and made available.

## Vignettes

Because the R packages `rmarkdown` and `knitr` are used to build this vignette to HTML output, you are required to have these installed to view vignettes available from this package. Both of these packages can be installed and loaded using the directions above for `devtools`. Vignettes can then be accessed using:
```
browseVignettes("telemetyr")
```

Alternatively, vignettes can be viewed in the Help menu using, for example:
```
vignette("Data_Prep", package = "telemetyr")
```

## `postpack`

A quick shout out to the `postpack` R package available [here](https://github.com/bstaton1/postpack). The `telemetyr` package invokes `postpack` for working with the \code{mcmc.list} object resulting from the CJS model performed using JAGS. `postpack` should be installed with the `telemetyr` package, but can also be installed using:
```
devtools::install_github("bstaton1/postpack")
```

## Developers Note

To use `devtools` you may also have to download and install Rtools. The latest version of Rtools can be found [here](https://cran.r-project.org/bin/windows/Rtools/).

## Making Contributions

If you are interested in making contributions to `telemetyr`, consider getting a GitHub account, fork this repository, clone to a local directory, modify, and send me a pull request. The authors can then review any changes and merge.

## Questions?

Please feel free to post an issue to this repository for requested features, bug fixes, errors in documentation, etc.

### Cheers!
