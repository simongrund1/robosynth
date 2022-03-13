\name{masking.model}
\alias{masking.model}

\title{Define masking models}

\description{
Defines a masking model for the inclusion in the synthesis.
}

\usage{
masking.model(formula, type, mask = NULL, ...)
}

\arguments{
\item{formula}{formula: model formula}
\item{type}{character: variable type. Possible values are "continuous", "binary", and "categorical".}
\item{mask}{(optional): parameters of the masking model (see Details).}
\item{\dots}{(optional): additional arguments passed to the model fitting function (e.g., \cr\code{nnet::multinom} for \code{type = "categorical"}).}
}

\details{
This function defines a masking model for the inclusion in the synthesis.
Each masking model defines the relation between a variable and its masked copy as a \code{formula}, where the original variable is specified on the right-hand side of \code{formula} and the masked copy on the left-hand side (\code{`copy` ~ `original`}).
Each masking model must be accompanied by a synthesis model for the respective variable as defined with \code{\link{synthesis.model}}.
One or more of these models can be included in the synthesis by passing them to \code{\link{combine.models}}.

The parameters of the masking model correspond to those that were used to generate the masked copies (with \code{\link{mask.continuous}} or \code{\link{mask.categorical}}).
Specifying the parameters is optional.
If the parameters are not specified, \code{\link{combine.models}} will attempt to extract them from the data.

For usage examples, see \code{\link{combine.models}}.
}

\value{
An object of class \code{robosynth.masking.model}.
}

\author{Simon Grund}

\seealso{
\code{\link{combine.models}}, \code{\link{synthesis.model}}, \code{\link{mask.categorical}}, \code{\link{mask.continuous}}
}

