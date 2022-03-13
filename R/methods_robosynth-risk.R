# // S3 methods for identification risk objects

# * header

.print.risk.info <- function(x){

  cll <- x$call

  cat("\n")
  cat("Call:")
  cat("\n\n")
  cat(paste(deparse(cll), collapse = "\n"))

  cat("\n\n")
  cat("Known variables:", paste(x$variables$known, collapse = ", "))
  cat("\n")
  cat("Synthetic variables:", paste(x$variables$synthetic, collapse = ", "))
  cat("\n\n")

  invisible(NULL)

}

# * print

print.robosynth.risk <- function(x, ...) .print.risk.info(x)

# * summary

summary.robosynth.risk <- function(object, threshold = NULL, show.cases = 0, ...){

  # record-level risk
  case_risk <- data.frame(
    true_match = rowSums(object$K),
    expected_risk = rowMeans(object$R)
  )
  attr(case_risk, "n") <- show.cases

  # high-risk cases (per threshold)
  if (!is.null(threshold)) {
    hr <- colSums(object$R > threshold)
    case_risk$high_risk <- rowSums(object$R > threshold)
  } else {
    hr <- NULL
  }

  # output
  out <- list(
    call = object$call,
    variables = object$variables,
    n = object$n,
    k = object$k,
    m = object$m,
    expected_risk = object$expected_risk,
    true_match = object$true_match,
    false_match = object$false_match,
    high_risk = hr,
    case_risk = case_risk,
    threshold = threshold
  )

  class(out) <- "robosynth.risk.summary"
  return(out)

}

# * print.summary

print.robosynth.risk.summary <- function(x, digits = 3, ...) {

  n <- x$n
  m <- x$m
  emr <- x$expected_risk
  tm <- x$true_match
  fm <- x$false_match
  hr <- x$high_risk
  cr <- x$case_risk

  # print info
  .print.risk.info(x)

  # summary of global risk
  cat("Global risk (avg., across m = ", m," data sets):", sep = "")
  cat("\n\n")

  cat("Expected match risk: ")
  cat("M =", format(mean(emr), digits = digits, ...), "")
  if (m > 1) cat(paste0("[min. = ", format(min(emr), digits = digits, ...), ", max. = ", format(max(emr), digits = digits, ...), "]"))
  cat("\n")

  if (!is.null(x$threshold)) {
    cat(paste0("High-risk cases (> ", format(x$threshold, digits = digits, ...), "): "))
    cat("n =", format(mean(hr), digits = digits, ...), "")
    if (m > 1) cat(paste0("[min. = ", min(hr), ", max. = ", max(hr), "]"))
    cat("\n")
  }

  cat("True matches: ")
  cat("n =", format(mean(tm), digits = digits, ...), "")
  if (m > 1) cat(paste0("[min. = ", min(tm), ", max. = ", max(tm), "]"))
  cat("\n")

  cat("False matches: ")
  cat("n =", format(mean(fm), digits = digits, ...), "")
  if (m > 1) cat(paste0("[min. = ", min(fm), ", max. = ", max(fm), "]"))
  cat("\n")

  # record-level risks
  if (attr(cr, "n") > 0) {

    cat("\n")
    cat("Record-level risk:")
    cat("\n\n")

    nc <- min(nrow(cr), attr(cr, "n"))

    cat("Expected match risk: ")
    emr_id <- order(cr$expected_risk, decreasing = TRUE)[1:nc]
    cat("ID (M) =", paste0(paste0(emr_id, " (", sapply(cr$expected_risk[emr_id], format, digits = digits, ...), ")"), collapse = ", "))
    cat("\n")

    if (!is.null(x$threshold)) {
      cat(paste0("High-risk cases (> ", format(x$threshold, digits = digits, ...), "): "))
      hr_id <- order(cr$high_risk, decreasing = TRUE)[1:nc]
      cat("ID (n) =", paste0(paste0(hr_id, " (", sapply(cr$high_risk[hr_id], format, digits = digits, ...), ")"), collapse = ", "))
      cat("\n")
    }

    cat("True matches: ")
    tm_id <- order(cr$true_match, decreasing = TRUE)[1:nc]
    cat("ID (n) =", paste0(paste0(tm_id, " (", cr$true_match[tm_id], ")"), collapse = ", "))
    cat("\n")

  }

  cat("\n")

  invisible(NULL)

}

