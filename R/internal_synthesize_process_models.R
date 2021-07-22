# // Pre-processing of synthesis models

process_synthesis_models <- function(models, data, variables) {

  Nm <- length(models)
  Nc <- ncol(data)
  Nv <- length(variables)

  # create predictor matrix
  predictor_matrix <- matrix(0L, nrow = Nc, ncol = Nc)
  colnames(predictor_matrix) <- rownames(predictor_matrix) <- colnames(data)

  # fill predictor matrix with model contents
  for (mm in 1L:Nm) {

    name_mm <- names(models)[mm]
    model_mm <- models[[mm]]
      
    # substantive models
    if (inherits(model_mm, "robosynth.synthesis.model")) {

      vars_mm <- all.vars(model_mm$formula)
      predictor_matrix[name_mm, vars_mm] <- 1L

    }
    
    # masking models
    if (inherits(model_mm, "robosynth.masking.model")) {

      vars_mm <- c(name_mm, model_mm$variable)
      predictor_matrix[name_mm, vars_mm] <- 1L

    }

  }

  # create list of models associated with each variable
  synth_models <- vector("list", Nv)
  names(synth_models) <- variables

  # fill list of models
  for (vv in variables) {
    synth_models[[vv]] <- colnames(data)[predictor_matrix[, vv] == 1]
  }
    
  return(synth_models)

}

