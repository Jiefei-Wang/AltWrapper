################################
##altwrapper S3 method
################################
#' Print altWrapper vector values
#'
#' This function is a complement of the print function. It is able to
#' print an ALTREP object made by AltWrapper package. In case that the
#' data pointer is not available, the function will look at
#' ALTREP APIs in the order of `getRegion`, `getSubset`,
#' `getElement` to search for the available way to collect the data.
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
#' @rdname S3Print
#' @return No return value
#' @export
print.altWrapper <- function(x, ...) {
    if (!is.altWrapper(x))
        stop("The object is not created by AltWrapper package")
    x = removeWrapper(x)
    xData = getAltData1(x)
    xLength = length(x)
    className = getAltClassName(x)
    classType = getClassType(className)
    ## If the data pointer is accessable, use default method
    func = .getAltMethod(className = className, methodName = "getDataptr")
    if (!is.null(func) && !is.null(func(xData, x))) {
        print.default(x)
        return(invisible(x))
    }
    ## If the data pointer or null is accessable, define the pointer method
    ## and use the default print
    func = .getAltMethod(className = className, methodName = "getDataptrOrNull")
    if (!is.null(func) && !is.null(func(xData, x))) {
        setAltMethod(
            className = className,
            getDataptr = function(x, writable)
                func(x)
        )
        print.default(x)
        setAltMethod(className = className,
                     getDataptr = NULL)
        return(invisible(x))
    }
    ## Chunk settings
    maxPrint = getOption("max.print")
    printSize = min(maxPrint, xLength)
    chunkSize = 512
    chunkNum = ceiling(printSize / chunkSize)
    
    ## Create a temp variable
    if (classType == "real") {
        constructor = numeric
    } else{
        constructor = get(classType)
    }
    output = structure(constructor(printSize), class = "altWrapper")
    
    ## Get data from getRegion
    func = .getAltMethod(className = className, methodName = "getRegion")
    if (!is.null(func)) {
        regionVector = C_create_internal_altrep(classType, chunkSize)
        for (i in seq_len(chunkNum)) {
            start = (i - 1) * chunkSize
            len = func(xData, start + 1, chunkSize, regionVector, x)
            output[start + seq_len(len)] = regionVector[seq_len(len)]
        }
        print.default(output)
        return(invisible(x))
    }
    
    ## Get data from getSubset
    func = .getAltMethod(className = className, methodName = "getSubset")
    if (!is.null(func)) {
        for (i in seq_len(chunkNum)) {
            start = (i - 1) * chunkSize
            len = min(chunkSize, xLength - start)
            regionVector = func(xData, start + seq_len(len), x)
            output[start + seq_len(len)] = regionVector
        }
        print.default(output)
        return(invisible(x))
    }
    
    ## get Data from getElement
    func = .getAltMethod(className = className, methodName = "getElement")
    if (!is.null(func)) {
        for (i in seq_len(printSize)) {
            output[i] = func(xData, i, x)
        }
        print.default(output)
        return(invisible(x))
    }
    stop("The data is not available")
}


#' @rdname S3Print
#' @export
printAltWrapper <- function(x, ...) {
    print.altWrapper(x, ...)
}
