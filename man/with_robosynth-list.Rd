\name{with/within}
\alias{with.robosynth.list}
\alias{within.robosynth.list}

\title{Handle and analyze synthetic data}

\description{
Functions for handling and analyzing a list of synthetic data sets.
}

\usage{
\method{with}{robosynth.list}(data, expr, ...)
\method{within}{robosynth.list}(data, expr, ...)
}

\arguments{
\item{data}{list of synthetic data sets as returned by \code{\link{extract}}.}
\item{expr}{expression to be evaluated in each of the synthetic data sets.}
\item{\dots}{not used.}
}

\details{
These functions can be used for data handling and for fitting statistical models in a list of synthetic data sets.
The \code{with} function can be used to fit statistical models, where \code{expr} is a call to a model fitting function (e.g., \code{lm} or similar; see Examples).
The \code{within} function can be used for various data handling tasks, for example, for transformations or to compute new variables (see Examples).

Formally, \code{with} evaluates \code{expr} in each data set and returns a list of results, whereas \code{within} evaluates \code{expr} in each data set and returns a list with the altered data sets.
}

\value{
For \code{with} an object of class \code{robosynth.result} (a list of the results of the evaluated expression).
For \code{within} an object of class \code{robosynth.list} (a list of synthetic data sets).
}

\author{Simon Grund}

\seealso{
\code{\link{extract}}
}

\examples{
# create masked copies
sociosexuality <- within(sociosexuality, {
  m_sex <- mask_categorical(sex, probability = .80)
  m_sexpref <- mask_categorical(sexpref, probability = .60)
  m_age <- mask_continuous(age, reliability = .90)
})

# combine synthesis and masking models
models <- combine_models(

  synthesis_model(sex ~ 1, type = "binary"),
  synthesis_model(sexpref ~ 1 + sex, type = "categorical"),
  synthesis_model(age ~ 1 + sex + sexpref, type = "continuous"),

  masking_model(m_sex ~ sex, type = "binary"),
  masking_model(m_sexpref ~ sexpref, type = "categorical"),
  masking_model(m_age ~ age, type = "continuous"),

  data = sociosexuality

)

# run synthesis
syn <- synthesize(models = models, m = 5, iter = 5)

# extract synthetic data
synlist <- extract(syn)

# * Example 1: centering (within)

# centering
synlist <- within(synlist, {
  c_age <- age - mean(age)
})

# * Example 2: analysis (with)

# fit linear model
fit <- with(synlist, lm(behavior ~ age))
}

\keyword{methods}
