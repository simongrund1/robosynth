# // Extract list of synthetic data sets

extract <- function(object, drop = TRUE, shuffle = FALSE) {

  syn_list <- object$syn

  N <- nrow(syn_list[[1]])
  m <- length(syn_list)

  # drop masked variables
  if (drop) {
    ind_mask <- sapply(object$models, inherits, "robosynth.masking.model")
    var_mask <- names(object$models)[ind_mask]
    vars <- !(colnames(syn_list[[1]]) %in% var_mask)
    for(jj in 1L:m) {
      syn_list[[jj]] <- syn_list[[jj]][, vars, drop = FALSE]
    }
  }

  # shuffle cases
  if (shuffle) {
    for(jj in 1L:m) {
      ind_row <- sample(1L:N, size = N)
      syn_list[[jj]] <- syn_list[[jj]][ind_row, ]
      rownames(syn_list[[jj]]) <- NULL
    }
  }

  class(syn_list) <- c("robosynth.list", "list")
  return(syn_list)

}

