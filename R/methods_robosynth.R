# // S3 methods for synthetic data objects

# * print

print.robosynth <- function(x, ...){

  # time
  cat("\nDate/Time: ", format(x$call_timestamp), "\n", sep = "")

  # call
  cat("\nCall:\n")
  call <- format(x$call)
  for (ii in seq_along(call)) cat(call[ii], "\n", sep = "")

  # model formulas
  cat("\nSynthesis Model:\n\n")
  for(mm in seq_along(x$models)) {
    type_mm <- x$models[[mm]]$type
    if (inherits(x$models[[mm]], "robosynth.masking.model")) {
      type_mm <- paste0("masking, ", type_mm)
    }
    model_mm <- deparse(x$models[[mm]]$formula)
    cat(model_mm, " (", type_mm, ")", "\n", sep = "")
  }
  cat("\n")

  invisible(NULL)

}

# * summary

summary.robosynth <- function(object, ...) {

  UseMethod("print", object)

}

