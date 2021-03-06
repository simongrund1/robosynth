\name{combine.models}
\alias{combine.models}

\title{Combine synthesis and masking models}

\description{
Combines and checks the synthesis and masking models to be included in the synthesis.
}

\usage{
combine.models(..., data)
}

\arguments{
\item{\dots}{one or more models of class \code{robosynth.synthesis.model} as returned by \code{\link{synthesis.model}} or \code{robosynth.masking.model} as returned by \code{\link{masking.model}}.}
\item{data}{data frame: the original data, for which synthetic data should be generated.}
}

\details{
This function combines one or more synthesis and masking models into a combined model that can be passed to \code{\link{synthesize}} to generate the synthetic data.
In addition, the function will perform checks and attempt to compute the values for the masking and proposal parameters in the masking and synthesis models from the data, if they were not specified by the user (throwing an error if this fails).
}

\value{
An object of class \code{robosynth.model.list}, containing the combined specification of the synthesis and masking models.
}

\author{Simon Grund}

\seealso{
\code{\link{synthesis.model}}, \code{\link{masking.model}}, \code{\link{synthesize}}
}

\examples{
# create of masked copies
sociosexuality <- within(sociosexuality, {
  m_sex <- mask.categorical(sex, probability = .80)
  m_sexpref <- mask.categorical(sexpref, probability = .60)
  m_age <- mask.continuous(age, reliability = .90)
})

# combine synthesis and masking models
combine.models(

  synthesis.model(sex ~ 1, type = "binary"),
  synthesis.model(sexpref ~ 1 + sex, type = "categorical"),
  synthesis.model(age ~ 1 + sex + sexpref, type = "continuous"),

  masking.model(m_sex ~ sex, type = "binary"),
  masking.model(m_sexpref ~ sexpref, type = "categorical"),
  masking.model(m_age ~ age, type = "continuous"),

  data = sociosexuality

)
}

