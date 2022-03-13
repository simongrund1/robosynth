# // Synthesis model specification (synthesis model constructor)

synthesis.model <- function(formula, type, fixed = FALSE, proposal = NULL, ...) {

  type <- match.arg(type, choices = c("continuous", "binary", "categorical"))
  dots <- list(...)

  # check input
  check.model.formula(formula)

  if (type != "continuous" && !is.null(proposal)) {
    warning("The 'proposal' argument only applies to continuous variables.")
  }

  # make model
  model <- list(
    type = type, outcome = all.vars(formula)[1], formula = formula,
    fixed = fixed, proposal = proposal, args = dots
  )

  class(model) <- c("robosynth.synthesis.model", "robosynth.model")
  return(model)

}

