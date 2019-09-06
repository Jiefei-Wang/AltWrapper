#' ALTREP macro
#' 
#' @details 
#' `altrepSortStatus` is a list containing all sortness status of an ALTREP object.
#' It can be used as the return value of `isSorted` function when defining
#' an ALTREP class. The sortness status can be unknown sortness, 
#' known unsorted, ascending, descending, ascending with NA first,
#' descending with NA first.
#' 
#' Similarly, `altrepNAStatus` contains all possible NA status of an ALTREP object.
#' The current available status are unknow and no NA.
#' 
#' 
#' @name altrepMacro
#' @aliases altrepSortStatus altrepNAStatus altrepMacro
#' @export altrepSortStatus altrepNAStatus
NULL



#' Check the type of an object
#'
#' @param x An object
#' @details
#' `is.altrep` : Is an object ALTREP?
#' @rdname typeCheck
#' @examples
#' ## An R vector is not ALTREP nor altWrapper.
#' x=runif(10)
#' is.altrep(x)
#' is.altWrapper(x)
#'
#' ## A compact sequence is ALTREP but not altWrapper.
#' x=1:10
#' is.altrep(x)
#' is.altWrapper(x)
#' @return Logical value indicating whether the object is an ALTREP
#' @export
is.altrep <- function(x) {
    C_ALTREP(x)
}



#' ALTREP API
#'
#' Get / Set data1 or data2 from an ALTREP object.
#' The function will return `NULL` if the object is
#' not an ALTREP. These are native ALTREP APIs, it is
#' not recommended to call these functions to modify an
#' ALTREP object.
#'
#' @param x An ALTREP object
#' @param value The data that will be set to an ALTREP object
#' @examples
#' ## A compact sequence is an ALTREP,
#' ## so ALTREP API can get the data that supports the compact sequence.
#' ## The data is for developer only and not supposed to be used by users.
#' x=1:10
#' getAltData1(x)
#' getAltData2(x)
#'
#' @return
#' `getAltData1` : An R object in data1 slot of the ALTREP
#' @rdname altrep-api
#' @export
getAltData1 <- function(x) {
    if (!is.altrep(x))
        return(NULL)
    C_get_alt_data1(x)
}

#' @return
#' `getAltData2` : An R object in data2 slot of the ALTREP
#'
#' @rdname altrep-api
#' @export
getAltData2 <- function(x) {
    if (!is.altrep(x))
        return(NULL)
    C_get_alt_data2(x)
}

#' @return
#' `setAltData1` : `TRUE` if the value has been set or
#' `FALSE` if the object is not an ALTREP
#' @rdname altrep-api
#' @export
setAltData1 <- function(x, value) {
    if (!is.altrep(x))
        return(FALSE)
    C_set_alt_data1(x, value)
    invisible(TRUE)
}
#' @return
#' `setAltData2` : `TRUE` if the value has been set or
#' `FALSE` if the object is not an ALTREP
#' @rdname altrep-api
#' @export
setAltData2 <- function(x, value) {
    if (!is.altrep(x))
        return(FALSE)
    C_set_alt_data2(x, value)
    invisible(TRUE)
}





