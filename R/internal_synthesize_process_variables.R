# // Pre-processing of target variables

process.variables <- function(data, variables) {

  Nv <- length(variables)

  variable_summary <- vector("list", Nv)
  names(variable_summary) <- variables

  for (vv in 1L:Nv) {

    var_vv <- variables[vv]

    # continous
    if (is.numeric(data[[var_vv]])){
      type_vv <- "numeric"
    }

    # categorical
    if (is.factor(data[[var_vv]])) {

      if (nlevels(data[[var_vv]]) <= 2L) {
        type_vv <- "binary"
      } else {
        type_vv <- ifelse(is.ordered(data[[var_vv]]), "ordered", "categorical")
      }

    }

    variable_summary[[var_vv]]$type <- type_vv

    # category information
    if (type_vv %in% c("binary", "ordered", "categorical")) {

      variable_summary[[var_vv]]$levels <- levels(data[[var_vv]])
      variable_summary[[var_vv]]$n_levels <- nlevels(data[[var_vv]])
      variable_summary[[var_vv]]$prob_levels <- as.matrix(table(data[[var_vv]]))[, 1] / nrow(data)

    }

  }

  return(variable_summary)

}

