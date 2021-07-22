# // Synthesis model specification (internals)

check_model_formula <- function(formula, max.vars = NULL) {

  term <- terms(formula)
  vars <- all.vars(formula)

  # check for response
  if (attr(term, "response") == 0) stop("Model must include an outcome variable.")

  # check for "."
  if (any(vars == ".")) stop("Model must not include '.'.")

  # check number of variables (masking)
  if (!is.null(max.vars)) {
    if (length(vars) > max.vars) stop("Too many terms in model formula (max. ", max.vars, ").")
  }

  invisible(NULL)

}

