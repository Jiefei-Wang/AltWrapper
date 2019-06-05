#' @useDynLib AltWrapper, .registration = TRUE
# @importFrom Rcpp sourceCpp

.onUnload <- function(libpath) {
  library.dynam.unload("AltWrapper", libpath)
}

