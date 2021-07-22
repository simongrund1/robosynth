\name{synthesize}
\alias{synthesize}

\title{Synthesize data}

\description{
Synthesizes data with the data-augmented multiple imputation (DA-MI) approach by Jiang et al. (2021).
}

\usage{
synthesize(models, m = 10, iter = 100, verbose = TRUE)
}

\arguments{
  \item{models}{specification of the combined synthesis and masking models as returned by \code{\link{combine_models}}.}
  \item{m}{integer: the number of synthetic data sets.}
  \item{iter}{integer: the number of iterations per synthetic data set.}
  \item{verbose}{logical: if \code{TRUE} (the default), the progress is printed to the console.}
}

\details{
This function synthesizes data with the data-augmented multiple imputation (DA-MI) approach by Jiang et al. (2021).
The synthesis model is specified with \code{\link{combine_models}} and consists of one or more synthesis models (see \code{\link{synthesis_model}}) that define the relations between the observed variables and a number of masking models (see \code{\link{masking_model}}) that define the relations between the original variables and masked copies thereof.

Several functions can be used to process the synthetic data.
The \code{\link{extract}} function can be used to extract the synthetic data from the synthetic data object.
The \code{\link{with.robosynth.list}} function can be used to analyze the synthetic data with conventional statistical methods (e.g., \code{lm}, \code{glm} etc.).
}

\value{
An object of class \code{robosynth}.
}

\references{
Jiang, B., Raftery, A. E., Steele, R. J., & Wang, N. (2021). Balancing inferential integrity and disclosure risk via model targeted masking and multiple imputation. \emph{Journal of the American Statistical Association}. Advance online publication. \doi{10.1080/01621459.2021.1909597}
}

\author{Simon Grund, Alexander Robitzsch}

\seealso{
\code{\link{combine_models}}, \code{\link{synthesis_model}}, \code{\link{masking_model}}, \code{\link{with.robosynth.list}},\cr\code{\link{extract}}
}

\examples{
# NOTE: The number of syntheses (m) and iterations (iter) used here are
# meant to minimize the run time of the examples and should be set to
# larger values in practice (e.g., m = 10, iter = 100).

# * Example 1: DA-MI

# create of masked copies
sociosexuality <- within(sociosexuality, {
  m_sex <- mask_categorical(sex, probability = .80)
  m_sexpref <- mask_categorical(sexpref, probability = .60)
  m_age <- mask_continuous(age, reliability = .90)
  m_behavior <- mask_continuous(behavior, reliability = .95)
  m_attitude <- mask_continuous(attitude, reliability = .95)
  m_desire <- mask_continuous(desire, reliability = .95)
})

# combine synthesis and masking models
models <- combine_models(

  synthesis_model(sex ~ 1, type = "binary"),
  synthesis_model(sexpref ~ 1 + sex, type = "categorical"),
  synthesis_model(age ~ 1 + sex + sexpref, type = "continuous"),
  synthesis_model(behavior ~ 1 + sex + sexpref + age, type = "continuous"),
  synthesis_model(attitude ~ 1 + sex + sexpref + age + behavior,
                  type = "continuous"),
  synthesis_model(desire ~ 1 + sex + sexpref + age + behavior + attitude,
                  type = "continuous"),

  masking_model(m_sex ~ sex, type = "binary"),
  masking_model(m_sexpref ~ sexpref, type = "categorical"),
  masking_model(m_age ~ age, type = "continuous"),
  masking_model(m_behavior ~ behavior, type = "continuous"),
  masking_model(m_attitude ~ attitude, type = "continuous"),
  masking_model(m_desire ~ desire, type = "continuous"),

  data = sociosexuality

)

# run synthesis
syn <- synthesize(models = models, m = 5, iter = 5)

# * Example 2: MI (conventional synthesis)

# combine synthesis and masking models
models <- combine_models(

  synthesis_model(sex ~ 1, type = "binary"),
  synthesis_model(sexpref ~ 1 + sex, type = "categorical"),
  synthesis_model(age ~ 1 + sex + sexpref, type = "continuous",
                  proposal = 5.0),
  synthesis_model(behavior ~ 1 + sex + sexpref + age, type = "continuous",
                  proposal = 2.0),
  synthesis_model(attitude ~ 1 + sex + sexpref + age + behavior,
                  type = "continuous", proposal = 2.0),
  synthesis_model(desire ~ 1 + sex + sexpref + age + behavior + attitude,
                  type = "continuous", proposal = 2.0),

  data = sociosexuality

)

# run synthesis
syn <- synthesize(models = models, m = 5, iter = 5)
}
