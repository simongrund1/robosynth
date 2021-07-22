# // Masking (continuous variables)

mask_continuous <- function(x, reliability, .sd) {

  # check input
  if (missing(reliability) == missing(.sd)) {
    stop("Either 'reliability' or '.sd' must be specified, but not both.")
  }

  # compute SD from reliability
  if (!missing(reliability)) {
    if (!is.numeric(reliability) || !(reliability > 0 && reliability < 1) || length(reliability) > 1) {
      stop("The 'reliability' must be a number between 0 and 1.")
    }
    .sd <- sqrt(1/reliability - 1) * stats::sd(x, na.rm = TRUE)
  } 

  # create masked copies
  z <- stats::rnorm(length(x), mean = x, sd = .sd)

  attr(z, "masking_sd") <- .sd
  return(z)

}

