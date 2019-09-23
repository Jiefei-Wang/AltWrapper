################################
##Class definition
################################
altClassList <- data.frame(
    type = c("raw", "logical", "integer", "numeric"),
    className = c("altRaw", "altLogical", "altInteger", "altReal"),
    stringsAsFactors = FALSE
)


## Make S3 class compatible with S4 dispatching
for (i in seq_len(nrow(altClassList))) {
    setOldClass(c(altClassList$className[i], altClassList$type[i]))
}


#' @title The S4 class for ALTREP of type raw, logical, integer and real
#'
#' @name altwrapper-S4class
#' @rdname S4AltWrapper
NULL

#' @rdname S4AltWrapper
#' @export
setClass(Class = "altRaw",
         contains = "raw")
#' @rdname S4AltWrapper
#' @export
setClass(Class = "altLogical",
         contains = "logical")
#' @rdname S4AltWrapper
#' @export
setClass(Class = "altInteger",
         contains = "integer")
#' @rdname S4AltWrapper
#' @export
setClass(Class = "altReal",
         contains = "numeric")


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
#' A=makeAltrep(className = "example", x = 1L:10L)
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
NULL



#' @rdname print-function
#' @export
print.altRaw <- function(x, ...) {
    printAltWrapper(x)
}
#' @rdname print-function
#' @export
print.altInteger <- function(x, ...) {
    printAltWrapper(x)
}
#' @rdname print-function
#' @export
print.altLogical <- function(x, ...) {
    printAltWrapper(x)
}
#' @rdname print-function
#' @export
print.altReal <- function(x, ...) {
    printAltWrapper(x)
}

## Set print dispatch for S4 class
for (i in seq_len(nrow(altClassList))) {
    setMethod("show", altClassList$className[i], function(object)
        printAltWrapper(object))
}


#' @rdname print-function
#' @export
printAltWrapper <- function(x, ...) {
    # browser()
    # message("My print")
    if (!is.altWrapper(x)) {
        return(NextMethod(generic = "print"))
    }
    
    x = removeWrapper(x)
    className = getAltClassName(x)
    classType = getClassType(className)
    ptr_func = .getAltMethod(className = className, methodName = "getDataptr")
    ptr_null_func = .getAltMethod(className = className, methodName = "getDataptrOrNull")
    if (!is.null(ptr_func)&&
        !is.null(ptr_null_func)&&
        !is.null(ptr_null_func(.getAltData1(x), x))) {
        return(NextMethod(generic = "print"))
    }
    
    
    ## Chunk settings
    maxPrint = getOption("max.print")
    printSize = min(maxPrint, length(x))
    chunkSize = 512
    chunkNum = ceiling(printSize / chunkSize)
    
    ## Create a temp variable
    constructor = get(toBaseRType(classType))
    output = structure(constructor(printSize))
    #attributes(output)=attributes(x)
    
    ## Print from region
    func = .getAltMethod(className = className, methodName = "getRegion")
    if (!is.null(func)) {
        regionVector = C_create_internal_altrep(classType, chunkSize)
        xData = .getAltData1(x)
        for (i in seq_len(chunkNum)) {
            start = (i - 1) * chunkSize
            len = func(xData, start + 1, chunkSize, regionVector, x)
            output[start + seq_len(len)] = regionVector[seq_len(len)]
        }
        print(output)
        return(invisible(x))
    }
    
    ## print from subset or element method
    output[seq_len(printSize)] = x[seq_len(printSize)]
    print(output)
    return(invisible(x))
}
