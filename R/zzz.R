.onAttach <- function(libname, pkgname){

  # Query version number from DESCRIPTION
  v <- utils::packageVersion("jdcR")

  # Startup message
  packageStartupMessage(paste("jdcR (version ", v ,")", sep=""))

}
