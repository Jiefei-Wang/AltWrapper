#' @useDynLib AltWrapper, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom DescTools StrAlign
#' @import BiocStyle

.onLoad<- function(libname, pkgname){
  C_initial_package(altrepRegistryEnvironment,altrepSymbolList)
}

.onUnload <- function(libpath) {
  library.dynam.unload("AltWrapper", libpath)
}
