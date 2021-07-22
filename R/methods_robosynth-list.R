# // S3 methods for lists of synthetic data sets

# * as

as.robosynth.list <- function(x) {

  # check list
  if (!is.list(x)) stop("Argument must be a 'list'.")

  # check entries
  is_df <- sapply(x, is.data.frame)
  if (any(!is_df)) {
    x <- lapply(x, as.data.frame)
    message("List entries were converted to class 'data.frame'.")
  }

  class(x) <- c("robosynth.list", class(x))
  return(x)
    
}

# * with

with.robosynth.list <- function(data, expr, ...) {

  expr <- substitute(expr)
  pf <- parent.frame()

  out <- lapply(data, function(d, expr, pf) eval(expr, d, pf), expr = expr, pf = pf)

  class(out) <- c("robosynth.result", "list")
  return(out)

}

# * within

within.robosynth.list <- function(data, expr, ...) {

  expr <- substitute(expr)
  pf <- parent.frame()

  out <- vector("list", length(data))
  for (dd in seq_along(data)) {

    out[[dd]] <- data[[dd]]

    # evlauate in local environment
    e <- evalq(environment(), data[[dd]], pf)
    eval(expr, e)

    # handle nulls
    l <- as.list(e)
    l <- l[!sapply(l, is.null)]

    # update data
    names_l <- names(l)
    out[[dd]][names_l] <- l

    # handle deletions
    names_del <- setdiff(names(data[[dd]]), names_l)
    Ndel <- length(names_del)
    if (Ndel > 0) {
      out[[dd]][names_del] <- if (Ndel == 1) NULL else vector("list", Ndel)
    }

  }

  class(out) <- c("robosynth.list", "list")
  return(out)

}

