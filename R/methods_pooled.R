# // S3 methods for pooled results

# * print

print.robosynth.pooled <- function(x, digits = 5, ...) {

  est <- x$estimates
  out <- matrix("", nrow = nrow(est) + 1, ncol = 5)

  # names
  out[1, -1] <- c("Estimate", "Std.Error", "t", "Pr(>|t|)")
  out[-1, 1] <- rownames(est)

  out[-1, 2] <- format(est[, "est"], digits = digits, ...)
  out[-1, 3] <- format(est[, "se"], digits = digits, ...)
  out[-1, 4] <- format(est[, "t_val"], digits = digits, ...)
  out[-1, 5] <- format(est[, "p_val"], digits = digits, ...)

  # fix width and justify
  out[, 1] <- format(out[, 1], justify = "left")
  for (jj in 2L:5L) out[, jj] <- format(out[, jj], justify = "right")

  # print to console
  cat("\nPooled Coefficients:\n")
  for (ii in 1L:nrow(out)) {
    cat(out[ii, ], sep = " ")
    cat("\n")
  }
  cat("\n")

  invisible(NULL)

}

# * summary

summary.robosynth.pooled <- function(object, ...) {

  UseMethod("print", object)

}

