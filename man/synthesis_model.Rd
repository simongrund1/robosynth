\name{synthesis.model}
\alias{synthesis.model}

\title{Define synthesis models}

\description{
Defines a synthesis model for the inclusion in the synthesis.
}

\usage{
synthesis.model(formula, type, fixed = FALSE, proposal = NULL, ...)
}

\arguments{
\item{formula}{formula: model formula}
\item{type}{character: variable type. Possible values are "continuous", "binary", and "categorical".}
\item{fixed}{logical: if \code{TRUE}, the variable is included in the synthesis as a (fixed) covariate without being synthesized.}
\item{proposal}{(optional) numeric: standard deviation of proposal distribution (only used if \code{type = "continuous"}; see Details).}
\item{\dots}{(optional): additional arguments passed to the model fitting function (e.g., \cr\code{nnet::multinom} for \code{type = "categorical"}).}
}

\details{
This function defines a synthesis model for the inclusion in the synthesis.
Each synthesis model includes between and outcome variable and any number of predictors as a formula \code{formula} (\code{`outcome ~  1 +  ...`}).
One or more of these models can be included in the synthesis by passing them to \code{\link{combine.models}}.

For continuous variables, the \code{proposal} argument can be used to specify the standard deviation of the proposal distribution used in the synthesis.
If \code{proposal} is not specified, \code{\link{combine.models}} will attempt to set it to a sensible value on the basis of the data.

For usage examples, see \code{\link{combine.models}}.
}

\value{
An object of class \code{robosynth.synthesis.model}.
}

\author{Simon Grund}

\seealso{
\code{\link{combine.models}}
}
