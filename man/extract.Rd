\name{extract}
\alias{extract}

\title{Extract list of synthetic data sets}

\description{
Extracts a list of synthetic data sets from a synthetic data object.
}

\usage{
extract(object, drop = TRUE, shuffle = FALSE)
}

\arguments{
\item{object}{object of class \code{robosynth} as returned by \code{\link{synthesize}}.}
\item{drop}{logical: if \code{TRUE} (the default), the masked copies are dropped from each data set.}
\item{shuffle}{logical: if \code{TRUE}, the rows in each data are shuffled (returned in a random order).}
}

\details{
This function extracts a list of synthetic data sets from a synthetic data object as returned by \code{\link{synthesize}}.
By default, the function drops the masked copies generated with \code{\link{mask_continuous}} and \code{\link{mask_categorical}} from the data set.
In addition, the rows of each data set can be shuffled to further anonymize the data.
}

\value{
An object of class \code{robosynth.list} (a list of data frames).
}

\author{Simon Grund}

\seealso{
\code{\link{synthesize}}, \code{\link{mask_categorical}}, \code{\link{mask_continuous}}
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

# * Example 1:

synlist <- extract(syn)

# * Example 2: shuffle data sets

synlist <- extract(syn, shuffle = TRUE)

# * Example 3: keep masked copies

synlist <- extract(syn, drop = FALSE)
}
