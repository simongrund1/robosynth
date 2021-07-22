# // Pooling of parameter estimates

pool <- function(model) {

  m <- length(model)

  # extract results
  qhat <- do.call(cbind, lapply(model, coef))
  uhat <- do.call(cbind, lapply(model, function(x) diag(vcov(x))))

  # pool estimates
  qbar <- rowMeans(qhat)
  ubar <- rowMeans(uhat)
  b <- apply(qhat, 1, var)
  v <- b / m + ubar
  se <- sqrt(v)

  r <- (b / ubar) / m
  nu <- (m - 1) * (1 + 1 / r)^2

  t_val <- qbar / se
  p_val <- 2 * pt(abs(t_val), df = nu, lower.tail = FALSE)

  # prepare output
  out <- list(
    estimates = cbind(est = qbar, se = se, df = nu, t_val = t_val, p_val = p_val)
  )

  class(out) <- "robosynth.pooled"
  return(out)

}

