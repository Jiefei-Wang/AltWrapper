#' @useDynLib AltWrapper, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom DescTools StrAlign
#' @import BiocStyle
#' @importFrom methods setMethod new
.onLoad <- function(libname, pkgname) {
    C_initial_package(altrepRegistryEnvironment)
    env = getNamespace("AltWrapper")
    altrepSortStatus = C_get_sortness_macro()
    altrepNAStatus = C_get_NA_status_macro()
    assign("altrepSortStatus", altrepSortStatus, envir = env)
    assign("altrepNAStatus", altrepNAStatus, envir = env)
}

.onUnload <- function(libpath) {
    library.dynam.unload("AltWrapper", libpath)
}
