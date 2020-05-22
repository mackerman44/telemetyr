# telemetyr

`telemetyr` is an R package to ease the burden of processing and interpreting oodles of telemetry observation data. More specifically, `telemetyr` aims to provide tools to ease the reading, cleaning, reduction, analysis, and visualization of telemetry observational data. The package is initially being developed as part of a radio telemetry project to characterize the distribution, movement, and survival of juvenile Chinook salmon in the Lemhi and Salmon rivers, Idaho, USA, during winter months, but the hope is that functions and tools within will eventually be developed further and can be applied elsewhere to any type of telemetry observation data.

At the moment, the package includes functionality to import, clean, and compress telemetry observations in preparation for analysis. Cleaned observations can then easily be turned into long- or wide-form capture histories which can then be used for data analysis and visualization. Some functions and documentation are also included to help the user run a Cormack-Jolly-Seber model. We also include functions that can be used for perform simple telemetry site "diagnostics" including evaluations of site operation times, noise, and the like.

`telemetyr` is currently designed to work with data downloaded from NOAA radio receivers and using the Tracker software, but our hope to is eventually improve `telemetyr` to accept data from a variety of receivers, be it radio, acoustic, or otherwise.

## Getting Started

NOTE: As of the initial writing of this README, `telemetyr` is still in development phase and may or may not be able to be installed as a package depending on the status of the build. The following will allow you to install and use `telemetyr` if the build is at a stable state:

To install `telemetyr` you can use Hadley Wickham's `devtools` package. To install and load `devtools` use:
```
install.packages("devtools")
library(devtools)
```

To use `devtools` you may also have to download and install Rtools. The latest version of Rtools can be found [here](https://cran.r-project.org/bin/windows/Rtools/).

Once `devtools` is successfully installed, use the following to install `telemtyr`:
```
devtools::install_github("mackerman44/telemetyr", build_vignettes = TRUE)
```

Be sure to include the `build_vignettes = TRUE` argument, as this will ensure that vignettes in the package will be made available. 

## JAGS

The `telemetyr` R package includes the functionality to create capture histories to be used in a Cormack-Jolly-Seber (CJS) capture-recapture model including a vignette on how to implement the model in
the JAGS (Just Another Gibbs Sampler) software. JAGS can be downloaded [here](https://sourceforge.net/projects/mcmc-jags/files/).

## `postpack`

In addition, we use the `postpack` R package available [here](https://github.com/bstaton1/postpack) for working with the class \code{mcmc.list} object resulting from the CJS performed in JAGS. To install the current working version of `postpack` you can use:
```
devtools::install_github("bstaton1/postpack")
```
The `build_vignettes = TRUE` option can also be invoked there to build vignettes upon installation.

## Vignettes

Because the R packages `rmarkdown` and `knitr` are used to build this vignette to HTML output, you are required to have these packages installed before running `install_github()` with the `build_vignettes = TRUE` argument. After building vignettes, they can then be accessed using the following, for example:

```
vignette("Data_Prep", package = "telemetyr")
```

## Making Contributions

If you are interested in making contributions to `telemetyr`, consider getting a GitHub account, fork this repository, clone to a local directory, modify, and send me a pull request. I can then review any changes and merge.

Cheers!
