\name{summary.robosynth.risk}
\alias{summary.robosynth.risk}

\title{Summarize global and record-level disclosure risks}

\description{
Function to summarize global and record-level disclosure risks in synthetic data.
}

\usage{
\method{summary}{robosynth.risk}(object, threshold = NULL, show.cases = 0, ...)
}

\arguments{
\item{object}{list of synthetic data sets as returned by \code{\link{extract}}.}
\item{threshold}{numeric: the threshold, above which cases are considered "high-risk". If \code{NULL} (the default), no measures based on high-risk cases are produced.}
\item{show.cases}{integer: the number cases included in the summary of record-level disclosure risks. If zero (the default), only global measures are produced.}
\item{\dots}{additional arguments (passed to \code{format}).}
}

\details{

This function provides summary measures for the risk of (identificiation) disclosure in synthetic data from the output of \code{\link{compute.risk}} in accordance with Reiter & Mitra (2009).
The available measures include global measures (per data set) and record-level measures (per case, if \code{show.cases >= 1}).

Global measures describe the overall risk of (identificiation) disclosure associated with each synthetic data set and include:

1. Expected match risk (i.e., the expected number of records that were correctly identified by either finding unique matches or guessing one of multiple matches)
2. High-risk cases (i.e., the number of records whose risk exceeded a given threshold)
3. True matches (i.e., the number of records that were correctly identified by finding unique matches in the synthetic data)
4. False matches (i.e., the number of records that were falsely identified by finding unique matches in the synthetic data)

Record-level measures describe the risk of (identification) disclosure for individual cases and include:

1. (Expected) match risk (i.e., the average contribution of each case to the globally expected match risk)
2. High-risk cases (i.e., the number of data sets, in which each case was considered high-risk)
3. True matches (i.e., the number of data sets, in which each case was correctly identified by searching for unique matches)

For the expected match rate, the number of high-risk cases, and the number of true matches, larger values represent higher risks.
For the number of false matches, larger values represent lower risks.

For additional computational details, see \code{\link{compute.risk}}.

}

\value{
An object of class \code{robosynth.risk.summary}.
}

\author{Simon Grund}

\seealso{
\code{\link{compute.risk}}
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
synlist <- extract(syn)

# compute risk (based on values in "age")
risk <- compute.risk(synlist, original = sociosexuality, synthetic = "age", width = .10)

# summarize risk with global and record-level measures
summary(risk, threshold = .20, show.cases = 3)
}

\keyword{methods}
