# // Miscellaneous internals

rowMax.matrix <- function(x) {

  res <- do.call(function(...) pmax(...), as.data.frame(x))
  return(res)

}

