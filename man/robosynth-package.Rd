\name{robosynth-package}
\alias{robosynth-package}

\docType{package}

\title{robosynth: Robust synthetic data generation with data-augmented multiple imputation}

\description{
Provides methods for synthetic data generation using the data-augmented multiple imputation (DA-MI) approach by Jiang et al. (2021) and tools for handling and analyzing synthetic data.

The DA-MI approach combines masking and imputation procedures by including masked copies of the original data in the synthesis model.
This package provides functions for creating masked copies of different variables types (\code{\link{mask_continuous}} and \code{\link{mask_categorical}}), specifying the synthesis model (\code{\link{synthesis_model}}, \code{\link{masking_model}}, and \code{\link{combine_models}}), generating synthetic data (\code{\link{synthesize}}), and analyzing the synthetic data (e.g., \code{with}, \code{pool}).
}

\references{
Jiang, B., Raftery, A. E., Steele, R. J., & Wang, N. (2021). Balancing inferential integrity and disclosure risk via model targeted masking and multiple imputation. \emph{Journal of the American Statistical Association}. Advance online publication. \doi{10.1080/01621459.2021.1909597}
}

\author{
Authors: Simon Grund, Alexander Robitzsch, Oliver Luedtke
}

\keyword{package}
