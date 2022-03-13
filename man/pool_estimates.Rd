\name{pool.estimates}
\alias{pool.estimates}

\title{Pool analysis results in synthetic data}

\description{
Pools the results of statistical analyses that were conducted with the synthetic data.
}

\usage{
pool.estimates(model)
}

\arguments{
\item{model}{list of statistical models (e.g., fitted by \code{\link{with.robosynth.list}}).}
}

\details{
This function pools the parameter estimates and standard errors of statistical models that have been fitted to the synthetic data (e.g., by \code{\link{with.robosynth.list}}) in accordance with the procedures in Reiter (2003).
}

\value{
An object of class \code{robosynth.pooled.estimates}.
}

\references{
Reiter, J. P. (2003). Inference for partially synthetic, public use microdata sets. \emph{Survey Methodology}, 29, 181-188. \url{https://www.statcan.gc.ca/}
}

\author{Simon Grund}

\seealso{
\code{\link{with.robosynth.list}}
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

# extract synthetic data
synlist <- extract(syn)

# fit model
fit <- with(synlist, lm(age ~ sex))

# pool results
pool.estimates(fit)
}
