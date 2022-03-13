\name{as.robosynth.list}
\alias{as.robosynth.list}

\title{Convert list of data sets}

\description{
Converts a list of synthetic data sets into a \code{robosynth.list}.
}

\usage{
as.robosynth.list(x)
}

\arguments{
\item{x}{list of data frames.}
}

\details{
This function converts a list of data sets into a \code{robosynth.list}.
If any of the list entries are not data frames, this function will attempt to convert them into data frames.
}

\value{
An object of class \code{robosynth.list} (a list of data frames).
}

\author{Simon Grund}

