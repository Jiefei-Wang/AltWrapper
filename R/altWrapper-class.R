#' @include altWrapper-internal.R
################################
##Class definition
################################
## Make S3 class compatible with S4 dispatching
for (i in c("raw", "logical", "integer", "double")) {
    setOldClass(getAltS3ClassVector(i))
}

#' The S3/S4 classes for ALTREP of type `raw`, `logical`, `integer` and `double`
#'
#' The S3/S4 classes for `raw`, `logical`, `integer` and `double` are `altRaw`, `altLogical`, 
#' `altInteger` and `altDouble` respectively. `altLogical`, `altInteger` and 
#' `altDouble` inherit `altNumeric` class. `altRaw` and `altNumeric` inherit `altWrapper` class.
#' `altWrapper` is a subclass of `ANY` type.
#'
#' @name altWrapperClass
#' @aliases altWrapper-class altNumeric-class altRaw-class 
#' altLogical-class altInteger-class altDouble-class
#' @rdname altWrapperClass
#' @export
setClass(Class = "altWrapper",
         contains = "ANY")
#' @rdname altWrapperClass
#' @export
setClass(Class = "altNumeric",
         contains = "altWrapper")
#' @rdname altWrapperClass
#' @export
setClass(Class = "altRaw",
         contains = "altWrapper")
#' @rdname altWrapperClass
#' @export
setClass(Class = "altLogical",
         contains = "altNumeric")
#' @rdname altWrapperClass
#' @export
setClass(Class = "altInteger",
         contains = "altNumeric")
#' @rdname altWrapperClass
#' @export
setClass(Class = "altDouble",
         contains = "altNumeric")

################################
##print method
################################

#' @title Print altWrapper vector values
#'
#' @description This function is a complement of the print function. It is able to
#' print an ALTREP object made by AltWrapper package. In case that the
#' data pointer is not available, the function will look at
#' ALTREP APIs in the order of `getRegion`, `getSubset`,
#' `getElement` to search for the available way to collect the data.
#'
#' @param x An altWrapper object
#' @param ... No effect, for compatibility only
#' @examples
## Define the ALTREP functions
#' length_func <- function(x) length(x)
#' element_function <- function(x,i) x[i]
#'
#' ## Define the altWrapper class and its functions
#' setAltClass(className = "example", classType = "integer")
#' setAltMethod(className = "example", getLength = length_func)
#' setAltMethod(className = "example", getElement = element_function)
#'
#'
#' ## Create an altWrapper object by providing the class name and data.
#' A <- newAltrep(className = "example", x = 1L:10L)
#'
#' ## The default print function does not wrok since it uses
#' ## data pointer only.
#' \dontrun{
#' A
#' }
#'
#' ## The package provide an alternative print function
#' printAltWrapper(A)
#'
#' @return The argument `x`
#' @name altWrapperPrint
#' @rdname print-function
#' @export
print.altWrapper <- function(x, ...) {
    toDefault = printAltWrapper(x)
    if (toDefault) {
        NextMethod()
    } else{
        invisible(NULL)
    }
}

## Set print dispatch for S4 class
setMethod("show", "altWrapper", function(object)
{
    toDefault = printAltWrapper(object)
    if (toDefault) {
        callNextMethod()
    } else{
        invisible(object)
    }
})

#' @rdname print-function
#' @export
printAltWrapper <- function(x, ...) {
    ## In generic call, the function return true if it wants to call
    ## the parent function, return false if it can handle the print
    ## In non-generic call, the function return the object x invisibly.
    .generic <- parent.frame(n = 1)[[".Generic"]]
    isGeneric <- !is.null(.generic)
    if (!is.altWrapper(x)) {
        if (isGeneric) {
            return(TRUE)
        } else{
            return(print(x, ...))
        }
    }
    
    x <- removeWrapper(x)
    className <- getAltClassName(x)
    classType <- getAltClassType(className)
    ptr_func <- .getAltMethod(className = className, methodName = "getDataptr")
    ptr_null_func <- .getAltMethod(className = className, methodName = "getDataptrOrNull")
    if (!is.null(ptr_func) &&
        !is.null(ptr_null_func) &&
        !is.null(ptr_null_func(.getAltData1(x), x))) {
        if (isGeneric) {
            return(TRUE)
        }
    }
    
    
    ## Chunk settings
    maxPrint <- getOption("max.print")
    printSize <- min(maxPrint, length(x))
    chunkSize <- getAltWrapperOptions("chunkSize")
    chunkNum <- ceiling(printSize / chunkSize)
    
    ## Create a temp variable
    constructor <- get(classType)
    if (isS4(x)) {
        output <- new(class(x), constructor(printSize))
    } else{
        output <- constructor(printSize)
        attributes(output) = attributes(x)
    }
    
    func <- .getAltMethod(className = className, methodName = "getRegion")
    if (!is.null(func)) {
        ## Print from region
        ## not a perfect solution but it should work reasonably fast.
        regionVector <- C_create_internal_altrep(classType, chunkSize)
        xData <- .getAltData1(x)
        for (i in seq_len(chunkNum)) {
            start <- (i - 1) * chunkSize
            len <- func(xData, start + 1, chunkSize, regionVector, x)
            output[start + seq_len(len)] <- regionVector[seq_len(len)]
        }
        res <- print(output)
    } else{
        ## print from subset or element method
        output[seq_len(printSize)] <- x[seq_len(printSize)]
        res <- print(output)
    }
    ## Print truncate information
    if (printSize > length(x)) {
        cat(
            '[ reached getOption("max.print") -- omitted ',
            length(x) - printSize,
            ' entries ]'
        )
    }
    
    if (isGeneric) {
        return(FALSE)
    } else{
        return(invisible(res))
    }
}
