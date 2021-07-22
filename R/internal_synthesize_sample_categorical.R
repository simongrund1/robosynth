# // Sampling for categorical data

sample_categorical <- function(n = nrow(probs), k = ncol(probs), probs) {

  u <- runif(n)

  if (k == 2L) {

    y <- u > probs[, 1]

  } else {

    y <- cp <- numeric(n)

    for (j in 1L:(k-1L)) {
      cp <- cp + probs[, j]
      y[u > cp] <- j
    }

  }
    
  return(y + 1L)

}

