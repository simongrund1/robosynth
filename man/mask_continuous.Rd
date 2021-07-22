\name{mask_continuous}
\alias{mask_continuous}

\title{Create masked copies of continuous variables}

\description{
Creates masked copies of continuous variables.
}

\usage{
mask_continuous(x, reliability, .sd)
}

\arguments{
\item{x}{a continuous variable.}
\item{reliability}{(optional) numeric: reliability of the masked copy. Larger values correspond to weaker masking (see Details).}
\item{.sd}{(optional) numeric: standard deviation of the noise added to the masked copy (see Details).}
}

\details{
This function creates masked copies of continuous variables.
The masking model generates the masked copy by adding noise to the original values
\deqn{
  x_i
}
%x_i^{\mathrm{masked}} = x_i + e
%%\quad \text{with} \quad
%%e_i \sim N(0,\sigma^2)
%%\; ,
where \eqn{\sigma^2} is the noise variance.

Either \code{reliability} or \code{.sd} (but not both) must be specified.
If \code{reliability} is specified, the masked copy is generated in such a way that it represents the original variable with the given reliability.
As an alternative, the \code{.sd} argument can be used to specify the noise standard deviation (\eqn{\sigma}) directly.
}

\value{
A numeric variable with masked values.
}

\author{Simon Grund}

\examples{
# using 'reliability'
sociosexuality <- within(sociosexuality, {
  m_age <- mask_continuous(age, reliability = .90)
})

# equivalent specification using '.sd'
sociosexuality <- within(sociosexuality, {
  m_age <- mask_continuous(age, .sd = 0.9253846)
})
}

