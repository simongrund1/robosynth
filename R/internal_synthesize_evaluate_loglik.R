# // Likelihood evaluation

evaluate_loglik <- function(model, outcome, data) {

  # continuous
  if (attr(model, "model_type") == "continuous") {
 
    y <- data[[outcome]]
    pred <- predict(model, newdata = data)
    ll <- stats::dnorm(y, mean = pred, sd = model$sigma, log = TRUE)
 
  }

  # binary
  if (attr(model, "model_type") == "binary") {
 
    y <- as.integer(data[[outcome]]) - 1L
    pred <- predict(model, type = "response", newdata = data)
    ll <- stats::dbinom(y, size = 1L, prob = pred, log = TRUE)
 
  }

  # categorical
  if (attr(model, "model_type") == "categorical") {
 
    N <- nrow(data)
    Nc <- nlevels(data[[outcome]])

    y <- as.integer(data[[outcome]])
    pred <- predict(model, type = "prob", newdata = data)

    if(Nc <= 2L){
      ll <- stats::dbinom(y - 1L, size = 1L, prob = pred, log = TRUE)
    }else{
      log_pred <- log(pred)
      ll <- log_pred[cbind(1L:N, y)]
    }
 
  }

  return(ll)

}

