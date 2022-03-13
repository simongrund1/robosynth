\name{shuffle}
\alias{shuffle}

\title{Shuffle list of data sets}

\description{
Shuffles the rows in a list of synthetic data sets
}

\usage{
shuffle(data)
}

\arguments{
\item{data}{list of synthetic data sets as returned by \code{\link{extract}} (or similar).}
}

\details{
This function shuffles the rows of a list of synthetic data sets and removes row names in the process.
This is intended to provide an additional layer of protection and (identification) disclosure attempts.

See also \code{\link{compute.risk}} and \code{\link{replace.high.risk}} for disclosure risk computation.
}

\value{
An object of class \code{robosynth.list} (a list of data frames).
}

\author{Simon Grund}

\seealso{
\code{\link{extract}}, \code{\link{compute.risk}}, \code{\link{replace.high.risk}}
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

# shuffle rows
synlist <- shuffle(synlist)
}
