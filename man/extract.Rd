\name{extract}
\alias{extract}

\title{Extract list of synthetic data sets}

\description{
Extracts a list of synthetic data sets from a synthetic data object.
}

\usage{
extract(object, drop = TRUE)
}

\arguments{
\item{object}{object of class \code{robosynth} as returned by \code{\link{synthesize}}.}
\item{drop}{logical: if \code{TRUE} (the default), the masked copies are dropped from each data set.}
}

\details{
This function extracts a list of synthetic data sets from a synthetic data object as returned by \code{\link{synthesize}}.
By default, the function drops the masked copies generated with \code{\link{mask.continuous}} and \code{\link{mask.categorical}} from the data set.
}

\value{
An object of class \code{robosynth.list} (a list of data frames).
}

\author{Simon Grund}

\seealso{
\code{\link{synthesize}}, \code{\link{mask.categorical}}, \code{\link{mask.continuous}}
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

# * Example 1:

synlist <- extract(syn)

# * Example 2: keep masked copies

synlist <- extract(syn, drop = FALSE)
}
