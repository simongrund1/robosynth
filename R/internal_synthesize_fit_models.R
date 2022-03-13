# // Model fitting

fit.synthesis.models <- function(models, data) {

  Nm <- length(models)

  # create list for fitted models
  fitted_models <- vector("list", Nm)
  names(fitted_models) <- names(models)

  for (mm in 1L:Nm) {

    model_mm <- models[[mm]]
    var_mm <- model_mm$outcome

    # * Synthesis models

    if (inherits(model_mm, "robosynth.synthesis.model")) {
    
      # continuous
      if (model_mm$type == "continuous") {

        # fit model
        fit_mm <- stats::lm(formula = as.formula(model_mm$formula), data = data)
        fit_mm$sigma <- stats::sigma(fit_mm)

        attr(fit_mm, "model_type") <- "continuous"

      }

      # binary
      if (model_mm$type == "binary") {

        # fit model
        fit_mm <- stats::glm(
          formula = as.formula(model_mm$formula), data = data,
          family = binomial(link = "logit")
        )

        attr(fit_mm, "model_type") <- "binary"

      }

      # categorical
      if (model_mm$type == "categorical") {

        # fit model
        cll <- call("multinom", 
          formula = as.formula(model_mm$formula), data = quote(data),
          trace = FALSE
        )
        if (length(model_mm$args) > 0) {
          for (aa in seq_along(model_mm$args)) {
            cll[[names(model_mm$args)[[aa]]]] <- model_mm$args[[aa]]
          }
        }
        fit_mm <- eval(cll)

        attr(fit_mm, "model_type") <- "categorical"

      }

    }

    # * Masking models

    if (inherits(model_mm, "robosynth.masking.model")) {
    
      # continuous
      if (model_mm$type == "continuous") {

        # fit preliminary model
        fit_mm <- stats::lm(formula = model_mm$formula, data = data)

        # fix coefficients
        fit_mm$coefficients <- c(0, 1)
        fit_mm$sigma <- model_mm$mask

        attr(fit_mm, "model_type") <- "continuous"

      }

      # binary
      if (model_mm$type == "binary") {

        # fit preliminary model
        fit_mm <- stats::glm(
          formula = model_mm$formula, data = data,
          family = binomial(link = "logit")
        )

        # compute coefficients
        mask_mm <- model_mm$mask
        b0 <- log(mask_mm[1, 2] / mask_mm[1, 1])
        b1 <- log(mask_mm[2, 2] / mask_mm[2, 1]) - b0

        # fix coefficients
        fit_mm$coefficients <- c(b0, b1)

        attr(fit_mm, "model_type") <- "binary"

      }

      # categorical
      if (model_mm$type == "categorical") {

        Nc <- nlevels(data[[var_mm]])

        # fit preliminary model
        cll <- call("multinom", 
          formula = as.formula(model_mm$formula), data = quote(data),
          trace = FALSE
        )
        if (length(model_mm$args) > 0) {
          for (aa in seq_along(model_mm$args)) {
            cll[[names(model_mm$args)[[aa]]]] <- model_mm$args[[aa]]
          }
        }
        fit_mm <- eval(cll)

        # compute coefficients
        mask_mm <- model_mm$mask
        b <- matrix(0, nrow = Nc - 1L, ncol = Nc)
        b[, 1] <- log(mask_mm[1, -1] / mask_mm[1, 1])
        for (cc in 2L:Nc) {
          b[, cc] <- log(mask_mm[cc, -1] / mask_mm[cc, 1]) - b[, 1]
        }

        # fix coefficients
        if (Nc <= 2L) {
          fit_mm$wts <- as.vector(rbind(0, t(b)))
        } else {
          fit_mm$wts <- as.vector(cbind(0, rbind(0, t(b))))
        }

        attr(fit_mm, "model_type") <- "categorical"

      }

    }
    
    fitted_models[[mm]] <- fit_mm

  }  

  return(fitted_models)

}

