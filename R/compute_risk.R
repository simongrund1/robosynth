compute.risk <- function(data, original, known, synthetic, width = .10,
                         relative = TRUE, tol = 1e-9){

  if (missing(known)) known <- character()
  if (missing(synthetic)) synthetic <- character()
  
  # * Setup -------------------------------------------------------------------

  # check input
  if (!is.list(data)) stop("The 'data' must be a list of synthetic data sets.")
  if (any(!sapply(data, is.data.frame))) stop("The 'data' must be a list of synthetic data sets.")
  if (any(!sapply(data, is.data.frame))) stop("The 'data' must be a list of synthetic data sets (data.frame).")
  if (!is.data.frame(original)) stop("The 'original' must be a data set (data.frame).")
  
  # sample size
  n <- nrow(original)
  if (any(sapply(data, function(d, n) nrow(d) != n, n = n))) stop("Sample sizes in 'data' and 'original' must be identical.")
  
  # variables
  vrs <- c(known, synthetic)
  k <- length(vrs)
  if (k == 0) stop("No 'known' or 'synthetic' variables were specified.")
  
  is_num <- sapply(data[[1]][, vrs, drop = FALSE], is.numeric)
  num_vrs <- names(which(is_num))
  cat_vrs <- names(which(!is_num))
  
  # check variable names
  for (vv in vrs) {
    if (!(vv %in% names(original))) stop(paste0("Variable '", vv, "' not found in 'original'"))
    if (any(!sapply(data, function(d, v) v %in% names(d), v = vv))) stop(paste0("Variable '", vv, "' not found in 'data'"))
  }
  
  # expand and check width
  if (length(width) > 1L) {
    if (length(width) != length(num_vrs)) stop("Length of 'width' does not match the number of continuous 'known' and 'synthetic' variables.")
    if (is.null(names(width))) {
      warning("Unnamed 'width' argument (using order implied by 'known' and 'synthetic').")
      names(width) <- num_vrs
    }
  } else {
    width <- rep(width, length(num_vrs))
    names(width) <- num_vrs
  }
  
  # expand and check width
  if (length(relative) > 1L) {
    if (length(relative) != length(num_vrs)) stop("Length of 'relative' does not match the number of continuous 'known' and 'synthetic' variables.")
    if (is.null(names(relative))) {
      warning("Unnamed 'relative' argument (using order implied by 'known' and 'synthetic').")
      names(relative) <- num_vrs
    }
  } else {
    relative <- rep(relative, length(num_vrs))
    names(relative) <- num_vrs
  }
  
  # compute lower and upper bounds for numeric variable matching
  num_lb <- num_ub <- matrix(NA_real_, nrow = n, ncol = length(num_vrs))
  colnames(num_lb) <- colnames(num_ub) <- num_vrs
  
  for (vv in num_vrs) {
    if (relative[vv]) {
      num_lb[,vv] <- original[,vv] * (1 - width[vv])
      num_ub[,vv] <- original[,vv] * (1 + width[vv])
    } else {
      num_lb[,vv] <- original[,vv] - width[vv]
      num_ub[,vv] <- original[,vv] + width[vv]
    }
  }
  
  # * Risk computation --------------------------------------------------------

  # find matches per target and data set
  cj <- Ij <- matrix(NA_integer_, nrow = n, ncol = length(data))
  
  for (mm in seq_along(data)) {
  
    for (jj in 1:n) {
  
      match_vv <- matrix(FALSE, nrow = n, ncol = k, dimnames = list(NULL, vrs))
  
      # NOTE: tol > 0 needed for compatibility with Hornby & Hu (2019, IRC v1.0.0),
      # where values on the boundary of the target range are non-matches, and for
      # floating point imprecision when relative = TRUE
      for (vv in num_vrs) match_vv[,vv] <- data[[mm]][, vv] > (num_lb[jj, vv] + tol) & data[[mm]][, vv] < (num_ub[jj, vv] - tol)
      for (vv in cat_vrs) match_vv[,vv] <- data[[mm]][, vv] == original[jj, vv]
  
      match_all <- .rowSums(match_vv, n, k) == k
      cj[jj, mm] <- sum(match_all)
      Ij[jj, mm] <- any(which(match_all) == jj)
  
    }
  
  }
  
  # * Format output -----------------------------------------------------------

  # compute record-level measures
  Rj <- Kj <- Fj <- matrix(0, nrow = n, ncol = length(data))
  c_ind <- cj > 0
  Rj[c_ind] <- Ij[c_ind] / cj[c_ind] # match risk
  Kj[cj == 1 & Ij == 1] <- 1         # true matches
  Fj[cj == 1 & Ij == 0] <- 1         # false matches
  
  # compute global measures
  emr <- colSums(Rj) # expected match risk
  tmr <- colSums(Kj) # true match risk
  fmr <- colSums(Fj) # false match risk
  
  # return output
  out <- list(
    call = match.call(),
    variables = list(known = known, synthetic = synthetic),
    n = n,
    k = k,
    m = length(data),
    width = width,
    relative = relative,
    c = cj,
    I = Ij,
    R = Rj,
    K = Kj,
    F = Fj,
    expected_risk = emr,
    true_match = tmr,
    false_match = fmr
  )
  
  class(out) <- "robosynth.risk"
  return(out)

}

