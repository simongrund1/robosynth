# robosynth

### Robust synthetic data generation with data-augmented multiple imputation

This [R](https://www.r-project.org/) package allows generating synthetic data using the data-augmented multiple imputation (DA-MI) approach by Jiang et al. ([2021](https://doi.org/10.1080/01621459.2021.1909597)).

Examples and additional information can be found in the [documentation](https://cran.r-project.org/package=mitml/mitml.pdf) of the package and [GitHub repo](https://github.com/simongrund1/robosynth).

This is an early build of `robosynth` with potentially significant changes between versions. Please report all bugs via the GitHub [issue](https://github.com/simongrund1/mitml/issues) tracker.

### Installation

#### CRAN

The package is currently not yet hosted on CRAN (planned for the future).

#### GitHub (development version)

The current development build of `robosynth` can be installed from GitHub using the [`devtools`](https://cran.r-project.org/package=devtools) package:

```r
install.packages("devtools")
devtools::install_github("simongrund1/robosynth")
```

![Github commits](https://img.shields.io/github/commits-since/simongrund1/robosynth/latest.svg?colorB=green)
