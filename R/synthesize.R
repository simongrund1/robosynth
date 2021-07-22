# // Synthesis with DA-MI

synthesize <- function(models, m = 10, iter = 100, verbose = TRUE) {

  # * Setup -------------------------------------------------------------------

  # process model input
  data <- attr(models, "data")
  variables <- attr(models, "variables")

  N <- nrow(data)
  Nv <- length(variables)

  syn_vars <- process_variables(data = data, variables = variables)
  syn_model_names <- process_synthesis_models(
    models = models, data = data, variables = variables
  )

  # fit models
  syn_models <- fit_synthesis_models(models = models, data = data)

  # create output objects
  syn_list <- vector("list", m)

  mh_vars <- variables[sapply(syn_models[variables], attr, "model_type") == "continuous"]
  mh_accept <- matrix(0, nrow = N, ncol = length(mh_vars))
  colnames(mh_accept) <- mh_vars

  # * Sampling ----------------------------------------------------------------

  for (jj in 1L:m) {

    for (ii in 1L:iter) {

      for (vv in seq_along(variables)) {

        # process relevant models
        var_vv <- variables[vv]
        type_vv <- attr(syn_models[[var_vv]], "model_type")
        model_names_vv <- syn_model_names[[var_vv]]

        # * Case 1: continuous (MH)

        if (type_vv == "continuous") {

          # propose new values
          new_data <- data
          new_data[[var_vv]] <- data[[var_vv]] + stats::rnorm(N, sd = models[[var_vv]]$proposal)

          # compute likelihood across relevant models
          loglik_new <- loglik_old <- numeric(N)

          for (mm in seq_along(model_names_vv)) {

            var_mm <- model_names_vv[mm]
            model_mm <- syn_models[[var_mm]]

            ll_old <- evaluate_loglik(model = model_mm, outcome = var_mm, data = data)
            ll_new <- evaluate_loglik(model = model_mm, outcome = var_mm, data = new_data)
            loglik_old <- loglik_old + ll_old
            loglik_new <- loglik_new + ll_new

          }

          # compute acceptance
          mh_ratio <- exp(loglik_new - loglik_old)
          acc <- mh_ratio > stats::runif(N)

          # store values
          mh_accept[, var_vv] <- mh_accept[, var_vv] + acc
          data[acc, var_vv] <- new_data[acc, var_vv]

        # * Case 2: categorical (Gibs)

        } else {

          Nc <- syn_vars[[var_vv]]$n_levels

          # compute likelihood for each category across relevant models
          loglik <- matrix(0, nrow = N, ncol = Nc)

          for (cc in 1L:Nc) {

            new_data <- data
            new_data[[var_vv]][] <- syn_vars[[var_vv]]$levels[cc]

            # loop over relevant models
            for (mm in seq_along(model_names_vv)) {

              var_mm <- model_names_vv[mm]
              model_mm <- syn_models[[var_mm]]

              ll <- evaluate_loglik(model = model_mm, outcome = var_mm, data = new_data)
              loglik[, cc] <- loglik[, cc] + ll

            }

          } # end loop (category)

          # compute normalized category probabilities
          loglik_max <- matrix(rep(rowMax_matrix(loglik), Nc), nrow = N, ncol = Nc)
          lik <- exp(loglik - loglik_max)
          lik_cons <- matrix(rep(rowSums(lik), Nc), nrow = N, ncol = Nc)
          p_post <- lik / lik_cons

          # sample values
          value_vv <- sample_categorical(n = N, k = Nc, probs = p_post)
          data[[var_vv]][] <- syn_vars[[var_vv]]$levels[value_vv]

        } # end if (cases)

      } # end loop (variable)

      # print progress
      if (verbose) {

        pr <- paste0("Synthesis ", jj, " | Iteration ", ii)
        print(pr)
        utils::flush.console()

      }

    } # end loop (iter)

    # save synthetic data set
    syn_list[[jj]] <- data

  } # end loop (m)

  # * Format output -----------------------------------------------------------

  # compute acceptance rate
  mh_accept_rate <- mh_accept / (iter * m)

  out <- list(
    call = match.call(),
    call_timestamp = Sys.time(),
    syn = syn_list,
    models = models,
    fitted_models = syn_models,
    accept = mh_accept_rate
  )

  class(out) <- "robosynth"
  return(out)

}

