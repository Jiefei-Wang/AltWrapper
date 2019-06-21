#' @useDynLib AltWrapper, .registration = TRUE
# @importFrom Rcpp sourceCpp

.onLoad<- function(libname, pkgname){
  C_initial_package(altrep_class_space,altrep_symbol_space)
  env=getNamespace(pkgname)
  assign("validFuncList",C_get_valid_func_name(),envir=env)
}

.onUnload <- function(libpath) {
  C_package_unload()
  library.dynam.unload("AltWrapper", libpath)
  
}
