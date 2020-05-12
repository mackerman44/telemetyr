# telemetyr

`telemetyr` is an R package to ease the burden of processing and interpreting oodles of telemetry observation data. More specifically, `telemetyr` aims to provide tools to ease the reading, cleaning, reduction, analysis, and visualization of telemetry observational data. The package is initially being built using data from a radio telemetry project to characterize the distribution, movement, and survival of juvenile Chinook salmon in the Lemhi and Salmon rivers, Idaho, during winter months, but the hope is that functions and tools within can be applied elsewhere to any type of telemetry data.

The project consists of two parts and functions and analyses within are build to support those:

1. Evaluation of the movement, distribution, and survival of Chinook salmon presmolts that winter in the mainstem Salmon River downstream of their natal Lemhi River. This life-history is alternatively termed as downstream rearing (DSR) juveniles. The `telemetyr` package contains functions to generate capture histories from raw telemetry data to be used, in our case, a Cormack-Jolly-Seber model.

2. Quantify habitat availablity and habitat use of juvenile Chinook salmon that remain in the Lemhi River to rear during winter months. This life history is alternatively termed as natal reach rearing (NRR) juveniles. The goal is that by comparing available versus used habitat we can infer habitat preference or target habitat conditions for juvenile Chinook winter rearing; this information can be used in habitat rehabilitation efforts. The `telemetyr` R package contains functions to summarize and analyze habitat use and availability.

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

## Making Contributions

If you are interested in making contributions to `telemetyr`, consider getting a GitHub account, fork this repository, clone to a local directory, modify, and send me a pull request. I can then review any changes and merge.

Cheers!
