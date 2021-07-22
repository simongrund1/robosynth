\name{mask_categorical}
\alias{mask_categorical}

\title{Create masked copies of categorical variables}

\description{
Creates masked copies of categorical variables.
}

\usage{
mask_categorical(x, probability, .matrix)
}

\arguments{
\item{x}{factor: a categorical variable (\code{factor} or \code{ordered}) with two or more categories (\code{levels}).}
\item{probability}{(optional) numeric: probability that the masked copy has the same value as the original variable. Larger values correspond to weaker masking (see Details).}
\item{.matrix}{(optional) matrix: masking matrix defining the probabilities of category retentions and transitions (see Details).}
}

\details{
This function creates masked copies of categorical variables.
The masking model is based on a masking matrix (also called a \emph{transition} matrix; Kuechenhoff et al., 2006), where the diagonal entries define the probabilities of category retentions (masked copy retains its original value), and the off-diagonal entries define the probabilities of category transitions (value on the masked copy differs from the original variable).

Either \code{probability} or \code{.matrix} (but not both) must be specified to define this matrix.
The value of \code{probability}, if specified, is used for the diagonal entries in the masking matrix, and the off-diagonal entries in each row are calculated in such a way that transitions into the other categories occur with equal probabilities and the row values sum up to one.
As an alternative, the \code{.matrix} argument can be used to specify the masking matrix directly.
}

\value{
A factor with masked values.
}

\references{
Kuechenhoff, H., Mwalili, S. M., & Lesaffre, E. (2006). A general method for dealing with misclassification in regression: The misclassification SIMEX. \emph{Biometrics}, 62, 85-96. \doi{10.1111/j.1541-0420.2005.00396.x}
}

\author{Simon Grund}

\examples{
# using 'probability'
sociosexuality <- within(sociosexuality, {
  m_sexpref <- mask_categorical(sexpref, probability = .60)
})

# equivalent specification using '.matrix'
M <- matrix(c(.60, .20, .20,
              .20, .60, .20,
              .20, .20, .60), 3, 3)

sociosexuality <- within(sociosexuality, {
  m_sexpref <- mask_categorical(sexpref, .matrix = M)
})
}

