# // Shuffle cases in list of synthetic data sets

shuffle <- function(data){

  N <- nrow(data[[1]])
  m <- length(data)

  # shuffle rows for each data set
  for (jj in 1L:m) {
    ind_row <- sample(1L:N, size = N)
    data[[jj]] <- data[[jj]][ind_row, ]
    rownames(data[[jj]]) <- NULL
  }

  class(data) <- c("robosynth.list", "list")
  return(data)

}
