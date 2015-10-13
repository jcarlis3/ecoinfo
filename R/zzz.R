.onAttach <- function(libname, pkgname){

  # Query version number from DESCRIPTION
  v <- utils::packageVersion("ecoinfo")

  # Startup message
  packageStartupMessage(paste("ecoinfo (version ", v ,")", sep=""))

}
