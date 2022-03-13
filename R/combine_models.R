# // Combine and check synthesis models

combine.models <- function(..., data) {

  # model list
  models <- list(...)
  names(models) <- sapply(models, "[[", "outcome")

  # * process masking models

  model_pairs <- list()

  for (mm in seq_along(models)) {

    var_mm <- names(models)[mm]
    model_mm <- models[[var_mm]]

    if (inherits(model_mm, "robosynth.masking.model")) {

      # check for missing synthesis models
      var_syn_mm <- model_mm$variable
      has_synthesis <- any(sapply(models, function(m, v) m$outcome == v, v = var_syn_mm))
      if (!has_synthesis) {
        stop("Each masking model requires a corresponding synthesis model, but no synthesis model was found for '", var_syn_mm, "'.")
      }

      # extract default masking parameters from data (if unspecified)
      if (is.null(model_mm$mask)) {
        mask_mm <- attr(data[[var_mm]], "masking")
        if (is.null(mask_mm)) stop("Could not determine masking parameter for '", var_mm, "'.")
        models[[var_mm]]$mask <- mask_mm
      }

      # save model pair
      model_pairs[[var_syn_mm]] <- model_mm$outcome

    }

  }

  # * process synthesis models

  vars_syn <- character()
  
  for (mm in seq_along(models)) {

    var_mm <- names(models)[mm]
    model_mm <- models[[var_mm]]

    if (inherits(model_mm, "robosynth.synthesis.model")) {

      # compute default proposal SD from masking SD (if unspecified)
      need_proposal <- !model_mm$fixed && model_mm$type == "continuous"
      if (need_proposal && is.null(model_mm$proposal)) {
        mask_mm <- models[[model_pairs[[var_mm]]]]$mask
        if (is.null(mask_mm)) stop("Could not determine proposal SD for '", var_mm, "'.")
        models[[var_mm]]$proposal <- mask_mm * sqrt(3.5)
      }

      # save variable names
      if (!model_mm$fixed) vars_syn <- c(vars_syn, var_mm)

    }

  }

  # add data and variable list
  attr(models, "data") <- data
  attr(models, "variables") <- vars_syn
  
  class(models) <- "robosynth.model.list"
  return(models)

}

