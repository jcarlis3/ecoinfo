.onAttach <- function(libname, pkgname){

  # Manually update the version number
  v <- "0.1.1"

  # Startup message
  packageStartupMessage(paste("jdcR (version ", v ,")", sep=""))

}
