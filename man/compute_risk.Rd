\name{compute.risk}
\alias{compute.risk}

\title{Compute identification disclosure risk}

\description{
Computes the risk of identification disclosure for a list of synthetic data sets.
}

\usage{
compute.risk(data, original, known, synthetic, width = .10, relative = TRUE, tol = 1e-9)
}

\arguments{
\item{data}{list of synthetic data sets as returned by \code{\link{extract}} (or similar).}
\item{original}{data.frame: original data set.}
\item{known}{character (optional): names of known (unsynthesized) variables to be used in the risk computation.}
\item{synthetic}{character (optional): names of synthetic variables to be used in the risk computation.}
\item{width}{numeric: scalar or a named vector determining the widths of the intervals for matching numeric variables.}
\item{relative}{logical: scalar or a named vector determining the type of intervals for numeric variables. If \code{TRUE} (the default), then \code{width} is used to construct relative (percentage) intervals of varying size (\code{x * (1 +/- width)}). Otherwise, \code{width} is used to construct intervals of fixed size (\code{x +/- width}).}
\item{tol}{numeric: numerical tolerance.}
}

\details{
This function computes the risk of identification disclosure for a list of synthetic data sets by attempting to match the values on the \code{known} and \code{synthetic} variables in the synthetic and original data.
For each target case, matches are identified by searching for matching cases with similar (if continuous) or identical values (if categorical) on the specified variables.
For continuous variables, cases are considered a match, if the true (unsynthesized) value falls into a certain interval around the synthetic value.

The size of this interval around continuous values is determined by \code{width}.
If \code{relative = TRUE} (the default), the interval around a given value is $x_i$ is $[x_i (1-w), x_i (1+w)]$, where $w$ is the specified width.
If \code{relative = FALSE}, the interval is $[x_i - w, x_i + w]$.
Both \code{width} and \code{relative} can be a named vector to use different intervals for different variables.

The result of the computation can be further summarized with \code{\link{summary.robosynth.risk}}, and high-risk cases can be protected further with \code{\link{replace.high.risk}}.
}

\value{
An object of class \code{robosynth.risk}.
}

\author{Simon Grund}

\seealso{
\code{\link{extract}}, \code{\link{summary.robosynth.risk}}, \code{\link{replace.high.risk}}
}

\examples{
# create masked copies
sociosexuality <- within(sociosexuality, {

  m_sex <- mask.categorical(sex, probability = .80)
  m_sexpref <- mask.categorical(sexpref, probability = .60)
  m_age <- mask.continuous(age, reliability = .90)

})

# combine synthesis and masking models
models <- combine.models(

  synthesis.model(sex ~ 1, type = "binary"),
  synthesis.model(sexpref ~ 1 + sex, type = "categorical"),
  synthesis.model(age ~ 1 + sex + sexpref, type = "continuous"),

  masking.model(m_sex ~ sex, type = "binary"),
  masking.model(m_sexpref ~ sexpref, type = "categorical"),
  masking.model(m_age ~ age, type = "continuous"),

  data = sociosexuality

)

# run synthesis
syn <- synthesize(models = models, m = 5, iter = 5)

# extract list of synthetic data sets
synlist <- extract(syn)

# * Example 1: matching by "age" with percentage intervals (10%)
compute.risk(synlist, original = sociosexuality, synthetic = "age")
# same as:
# compute.risk(synlist, original = sociosexuality, synthetic = "age", width = .10)
# compute.risk(synlist, original = sociosexuality, synthetic = "age", width = c(age = .10))

# * Example 2: matching by "sex", "sexpref", and "age" with fixed-width intervals for "age" (1 year)
compute.risk(synlist, original = sociosexuality, known = c("sex"), synthetic = "age", width = 1.0, relative = FALSE)
}
