replace.high.risk <- function(data, R, threshold, k = 1, match.variables, replace.variables,
                              n.replace, include = TRUE){

  # * Setup -------------------------------------------------------------------

  # check arguments
  if (n.replace <= 0) stop("'n.replace' must be an integer value greater than 0.")

  mat_vrs <- match.variables
  rep_vrs <- replace.variables

  vrs <- unique(c(mat_vrs, rep_vrs))
  is_num <- sapply(data[[1]][, vrs, drop = FALSE], is.numeric)
  num_vrs <- names(which(is_num))
  cat_vrs <- names(which(!is_num))

  mat_num_vrs <- mat_vrs[mat_vrs %in% num_vrs]
  mat_cat_vrs <- mat_vrs[mat_vrs %in% cat_vrs]

  # copy data
  n <- nrow(data[[1]])
  new_data <- data

  # * Replace values ----------------------------------------------------------

  # loop over data sets
  for (mm in seq_along(data)) {

    # find high-risk cases
    high_risk_mm <- which(R[,mm] > threshold)
    if (n.replace < length(high_risk_mm)) high_risk_mm <- sample(high_risk_mm, size = n.replace)

    # loop over cases
    for (jj in high_risk_mm) {

      # compute distance for categorical variables (best possible)
      if (length(mat_cat_vrs) > 0) {
        cat_mm <- data[[mm]][, mat_cat_vrs, drop = FALSE]
        cat_dist_jj <- unname(rowSums(cat_mm == data[[mm]][rep(jj, n), mat_cat_vrs, drop = FALSE]))
      } else {
        cat_dist_jj <- rep(0, n)
      }
      if (!include) cat_dist_jj[high_risk_mm] <- -Inf
      cat_dist_jj[jj] <- -Inf

      # compute distance on continuous variables (least squares)
      if (length(mat_num_vrs) > 0) {
        num_mm <- data[[mm]][, mat_num_vrs, drop = FALSE]
        num_dist_jj <- unname(rowSums((num_mm - data[[mm]][rep(jj, n), mat_num_vrs, drop = FALSE])^2))
      } else {
        num_dist_jj <- runif(n)
      }
      if (!include) num_dist_jj[high_risk_mm] <- Inf
      num_dist_jj[jj] <- Inf

      # find k records with lowest squared distance among best categorical matches
      cat_pos_jj <- unname(which(cat_dist_jj == max(cat_dist_jj)))
      n_pos_jj <- length(cat_pos_jj)
      if (n_pos_jj > k) {
        new_jj <- cat_pos_jj[order(num_dist_jj[cat_pos_jj])[1:k]]
      } else {
        new_jj <- cat_pos_jj
      }

      if (length(new_jj) > 1) new_jj <- sample(new_jj, size = 1)
      new_data[[mm]][jj, rep_vrs] <- data[[mm]][new_jj, rep_vrs]

    }

  }

  return(as.robosynth.list(new_data))

}

