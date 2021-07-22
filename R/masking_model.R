# // Synthesis model specification (masking model constructor)

masking_model <- function(formula, type, mask = NULL, ...) {

  type <- match.arg(type, choices = c("continuous", "binary", "categorical"))
  dots <- list(...)

  # check input
  check_model_formula(formula, max.vars = 2)

  if (type == "continuous") {
    if (!is.null(mask) && (!is.numeric(mask) || length(mask) > 1)) {
      stop("Invalid parameter in masking model.")
    }
  }
  if (type %in% c("binary", "categorical")) {
    if (!is.null(mask) && !is.matrix(mask)) stop("Invalid parameter in masking model.")
  }

  # make model
  model <- list(
    type = type, outcome = all.vars(formula)[1], variable = all.vars(formula)[2],
    formula = formula, mask = mask, args = dots
  )

  class(model) <- c("robosynth.masking.model", "robosynth.model")
  return(model)

}

