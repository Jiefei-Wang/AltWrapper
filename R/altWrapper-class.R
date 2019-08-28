################################
##Class definition
################################
altClassList<-data.frame(
    type=c("raw","logical","integer","numeric"),
    className=c("altRaw","altLogical","altInteger","altReal"),
    stringsAsFactors = FALSE
)


## Make S3 class compatible with S4 dispatching
for(i in seq_len(nrow(altClassList))){
    setOldClass(c(altClassList$className[i],altClassList$type[i]))
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


## Set print dispatch for S4 class
for (i in seq_len(nrow(altClassList))) {
    setMethod("show", altClassList$className[i], function(object)
        printAltWrapper(object))
}


#' @rdname print-function
#' @export
printAltWrapper <- function(x, ...) {
    #browser()
    if (!is.altWrapper(x)) {
        print(unclass(x))
        #stop("The object is not created by AltWrapper package")
        #class(x)=class(x)[!class(x)%in%altClassList$className]
        #return(print.default(x))
    }
    
    x = removeWrapper(x)
    className = getAltClassName(x)
    classType = getClassType(className)
    if (printFromPtr(x, className)) {
        return(invisible(x))
    }
    if (printFromPtrOrNuLL(x, className)) {
        return(invisible(x))
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
    
    
    if (printFromRegion(x, className, classType, output, chunkNum, chunkSize)) {
        return(invisible(x))
    }
    if (printFromSubset(x, className, output, chunkNum, chunkSize)) {
        return(invisible(x))
    }
    
    if (printFromElement(x, className, output, chunkSize, printSize)) {
        return(invisible(x))
    }
    stop("The data is not available")
}

printFromPtr <- function(x, className) {
    ## If the data pointer is accessable, use default method
    func = .getAltMethod(className = className, methodName = "getDataptr")
    # && !is.null(func(xData, x))
    if (!is.null(func)) {
        print(unclass(x))
        TRUE
    } else{
        FALSE
    }
}
## If the data pointer or null is accessable, define the pointer method
## and use the default print
printFromPtrOrNuLL <- function(x, className) {
    func = .getAltMethod(className = className, methodName = "getDataptrOrNull")
    if (!is.null(func)) {
        setAltMethod(
            className = className,
            getDataptr = function(x, writable)
                func(x)
        )
        print(unclass(x))
        setAltMethod(className = className,
                     getDataptr = NULL)
        TRUE
    } else{
        FALSE
    }
}

printFromRegion <-
    function(x,
             className,
             classType,
             output,
             chunkNum,
             chunkSize) {
        func = .getAltMethod(className = className, methodName = "getRegion")
        if (!is.null(func)) {
            regionVector = C_create_internal_altrep(classType, chunkSize)
            xData = getAltData1(x)
            for (i in seq_len(chunkNum)) {
                start = (i - 1) * chunkSize
                len = func(xData, start + 1, chunkSize, regionVector, x)
                output[start + seq_len(len)] = regionVector[seq_len(len)]
            }
            print.default(output)
            TRUE
        } else{
            FALSE
        }
    }

printFromSubset <- function(x, className, output, chunkNum, chunkSize) {
    ## Get data from getSubset
    func = .getAltMethod(className = className, methodName = "getSubset")
    if (!is.null(func)) {
        xData = getAltData1(x)
        xLength = length(x)
        for (i in seq_len(chunkNum)) {
            start = (i - 1) * chunkSize
            len = min(chunkSize, xLength - start)
            regionVector = func(xData, start + seq_len(len), x)
            output[start + seq_len(len)] = regionVector
        }
        print.default(output)
        TRUE
    } else{
        FALSE
    }
}
printFromElement <- function(x, className, output, chunkSize, printSize) {
    ## get Data from getElement
    func = .getAltMethod(className = className, methodName = "getElement")
    if (!is.null(func)) {
        xData = getAltData1(x)
        for (i in seq_len(printSize)) {
            output[i] = func(xData, i, x)
        }
        print.default(output)
        TRUE
    } else{
        FALSE
    }
}

#' @export
Ops.altInteger<-function(e1,e2){
    maxLen=max(length(e1),length(e2))
    if(length(e1)<length(e2)){
        tmp=e1
        e1=e2
        e2=tmp
    }
    j=1
    for(i in seq_len(maxLen)){
        e1[i]=e1[i]+e2[j]
        j=j+1
        if(j>length(e2)){
            j=1
        }
    }
    e1
}

# 
# getGeneric("Ops")
# 
# setClass(Class = "test",
#          contains = "numeric")
# 
# testFunc <- function(x) UseMethod("testFunc", x)
# testFunc.test<-function(x) message("S3")
# 
# setGeneric("testFunc",function(x)standardGeneric("testFunc"))
# setMethod("testFunc","matrix",function(x) message("S4"))
# 
# 
# 
# 
# obj=as.integer(c(1,2,3))
# 
# class(obj)=c("test","integer")
# 
# 
# library(IRanges)
# ir=IRanges(start=obj,width=10)
# start(ir)
# ir2=shift(ir,2L)
# 
# 
# obj_s4=new("test",obj)
# testFunc(obj)
# testFunc(obj_s4)
