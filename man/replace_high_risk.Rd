\name{replace.high.risk}
\alias{replace.high.risk}

\title{Replace high-risk cases in synthetic data}

\description{
Replaces values of high-risk cases in synthetic data sets with values from similar cases as determined by nearest-neighbor matching.
}

\usage{
replace.high.risk(data, R, threshold, k = 1, match.variables, replace.variables, n.replace, include = TRUE)
}

\arguments{
\item{data}{list of synthetic data sets as returned by \code{\link{extract}} (or similar).}
\item{R}{numeric: matrix of record-level risk estimates as returned by \code{\link{compute.risk}} (or similar).}
\item{threshold}{numeric: the threshold, above which cases are considered "high-risk"}
\item{k}{integer: number of neighbors from which replacements are sampled. The default is to replace with the nearest neighbor.}
\item{match.variables}{character: names of variables to be used in the search for nearest neighbors.}
\item{replace.variables}{character: names of variables to be replaced.}
\item{n.replace}{integer: (maximum) number of cases to replace in each data set.}
\item{include}{logical: if \code{TRUE} (the default), high-risk cases are included in the search for nearest neighbors. Otherwise, only low-risk cases are considered possible neighbors.}
}

\details{
This function can be used to replace cases that are at a high risk of identification disclosure with the values of similar cases in a list of synthetic data sets.
For each target case, replacement values are sampled randomly from its $k$ nearest neighbors, which have the most similar (or identical) configuration on categorical variables and similar (in terms of squared distance) values on continuous variables.

The primary purpose of this function is to reduce the risk of (identification) disclosure by replacing the values of a *small* number of high-risk cases on selected variables.
The number of cases that are considered "high-risk" is determined by \code{threshold}, and the number of replacements is determined by \code{n.replace}.
If the number of replacements is lower than the number of high-risk cases, then a random selection of high-risk cases will be replaced.

The result of the replacement algorithm is a list of synthetic data sets with a (usually) lower disclosure risk but also (usually) lower utility.
For this reason, it is good practice to replace only a small proportion of high-risk cases and to re-compute the risk of disclosure in the resulting data sets (see \code{\link{compute.risk}}).
}

\value{
An object of class \code{robosynth.list} (a list of data frames).
}

\author{Simon Grund}

\seealso{
\code{\link{extract}}, \code{\link{compute.risk}}
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

# compute risk for matching by "age"
risk <- compute.risk(synlist, original = sociosexuality, synthetic = "age")
summary(risk, threshold = .20, show.cases = 3)

# replace values in "age" using cases with similar "sex", "sexpref", and "age"
synlist.rep <- replace.high.risk(synlist, R = risk$R, threshold = .20, match.variables = c("sex", "sexpref", "age"), replace.variables = "sex", n.replace = 3)

# re-compute risk
risk.rep <- compute.risk(synlist.rep, original = sociosexuality, synthetic = "age")
summary(risk.rep, threshold = .20, show.cases = 3)
}
