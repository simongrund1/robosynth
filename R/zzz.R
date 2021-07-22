.onAttach <- function(libname, pkgname){

  msg <- paste(
    "This is an early build with potentially significant changes between versions.",
    "Please report all bugs via the GitHub issue tracker.",
    sep = "\n"
  )
  packageStartupMessage(msg)

}

