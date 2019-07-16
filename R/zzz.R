#' @useDynLib AltWrapper, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom DescTools StrAlign

.onLoad<- function(libname, pkgname){
  C_initial_package(altrepRegistryEnvironment,altrepSymbolList)
}

.onUnload <- function(libpath) {
  C_package_unload()
  library.dynam.unload("AltWrapper", libpath)
}
