globalSettings <- new.env()
globalSettings$redefineWarning = TRUE

#' AltWrapper Options Settings
#'
#' Get and set the package settings
#'
#' @param ... a list of named arguments to change the package settings,
#'  or a vector of character names to get the package settings. Currently the
#'  only available setting is `redefineWarning`, which controls whether an warning
#'  can be given when users try to redine a function or a class of an AltWrapper class.
#'
#' @examples
#' ## Get the default package settings
#' getAltWrapperOptions("redefineWarning")
#' ## change the settings
#' setAltWrapperOptions(redefineWarning = FALSE)
#' ## Get the settings again to see the change
#' getAltWrapperOptions("redefineWarning")
#'
#' @return 
#' `setAltWrapperOptions`: No return value 
#' `getAltWrapperOptions`: A list of package settings
#' @rdname AltWrapperOption
#' @export
setAltWrapperOptions <- function(...) {
    options = list(...)
    options = checkOptionExistance(options)
    for (i in seq_along(options)) {
        globalSettings[[names(options)[i]]] = options[[i]]
    }
}
#' @rdname AltWrapperOption
#' @export
getAltWrapperOptions <- function(...) {
    options = c(...)
    if (length(options) == 0) {
        return(as.list(globalSettings))
    }
    res = as.list(globalSettings)[options]
    if (length(options) == 1) {
        return(res[[1]])
    }
    return(res)
}

## Check if options exist or not
## return the options that exist
checkOptionExistance <- function(options) {
    noneExistOptions = !names(options) %in% names(globalSettings)
    if (any(noneExistOptions)) {
        vapply(paste0(names(options)[noneExistOptions]), function(x)
            warning("The option `", x, "` does not exist"), character(1))
    }
    options = options[!noneExistOptions]
    return(options)
}