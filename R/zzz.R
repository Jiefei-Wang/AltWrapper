#' @useDynLib AltWrapper, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom DescTools StrAlign
#' @import BiocStyle
#' @importFrom methods setMethod new
.onLoad<- function(libname, pkgname){
  C_initial_package(altrepRegistryEnvironment,altrepSymbolList)
}

.onUnload <- function(libpath) {
  library.dynam.unload("AltWrapper", libpath)
}
