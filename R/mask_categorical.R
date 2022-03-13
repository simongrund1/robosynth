# // Masking (categorical variables)

mask.categorical <- function(x, probability, .matrix) {

  # check input
  if (!is.factor(x)) stop("Categorical masking can only be used on factors.")
  if (missing(probability) == missing(.matrix)) {
    stop("Either 'probability' or '.matrix' must be specified, but not both.")
  }

  lvl <- levels(x)
  L <- nlevels(x)

  # create matrix from probability
  if (!missing(probability)) {

    if (!is.numeric(probability) || !(probability > 0 && probability < 1) || length(probability) > 1) {
      stop("The 'probability' must be a number between 0 and 1.")
    }
    .matrix <- matrix((1 - probability) / (L - 1), ncol = L, nrow = L)
    diag(.matrix) <- probability

  } 

  # check matrix
  sums <- rowSums(.matrix)
  if (any(abs(sums - 1) > 1e-7)) {
    warning("Rows in the masking 'matrix' did not sum up to 1. The entries were normalized.")
    for (rr in 1:nrow(.matrix)) {
      .matrix[rr, ] <- .matrix[rr, ] / sums[rr]
    }
  }

  # create masked copy
  z <- x
  for (ii in seq_along(x)) z[ii] <- sample(lvl, size = 1, prob = .matrix[x[ii], ])

  attr(z, "masking_matrix") <- .matrix
  return(z)

}

