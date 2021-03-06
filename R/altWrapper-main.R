################################
## AltWrapper global variables
################################
## This is the place to store all altWrapper definitions
altrepRegistryEnvironment <- new.env()
## The argument numbers of each function
## It is used to check if the function definition is correct
altrepClassFunctionArgNum <- c(
    inspect = 1,
    getLength = 1,
    getDataptr = 2,
    getDataptrOrNull = 1,
    getSubset = 2,
    getElement = 2,
    getRegion = 4,
    duplicate = 2,
    coerce = 2,
    serialize = 1,
    unserialize = 2,
    isSorted = 1,
    noNA = 1,
    sum = 2,
    min = 2,
    max = 2
)

altrepClassFunctionList <- names(altrepClassFunctionArgNum)

altWrapperClassDefaultSettings <- list(
    autoExportClassDef = TRUE,
    autoDuplicate = TRUE,
    autoSerialize = TRUE
)
################################
##Mian function
################################
#' Make an altWrapper object
#'
#' This function make an altWrapper object from an altWrapper class
#' specified by the class name. The altWrapper class can
#' be set via `setAltClass` and `setAltMethod` functions.
#'
#' @param className Character, the name of an altWrapper class
#' @param x The data of the returned altWrapper object
#' @param attributes Named list, attributes that will be attached to the object. The 
#' attribute `class` will be ignored
#' @param S3Class Logical or character vector, whether the return value is of an 
#' altWrapper S3 class. If the argument is a character vector, it will overwrite the 
#' default settings and resulting in an S3 object with the class type specified in the
#' character vector.
#' @param S4Class Logical or character, whether the return value is of 
#' an AltWrapper S4 class. If the argument is a character, it must be a class name and
#' the return value is an S4 object specified by the class name.
#' An error will be returned if both `S3Class` and `S4Class` are defined.
#' @examples
#' ## Define the ALTREP functions
#' length_func <- function(x) length(x)
#' get_ptr_func <- function(x,writeable) x
#'
#'
#' ## Define the altWrapper class and its functions
#' setAltClass(className = "example", classType = "integer")
#' setAltMethod(className = "example", getLength = length_func)
#' setAltMethod(className = "example", getDataptr = get_ptr_func)
#'
#' ## Create an altWrapper object by providing the class name and data.
#' A <- newAltrep(className = "example", x = 1L:10L)
#' A
#'
#'
#' @return An ALTREP vector
#' @export
newAltrep <- function(className,
                       x,
                       attributes = list(),
                       S3Class = FALSE,
                       S4Class = FALSE) {
    if (!isAltClassDefined(className)) {
        stop("The class '", className, "' is not found.")
    }
    
    if(is.null(attributes)){
        attributes <- list()
    }else{
        if (!is.list(attributes)){
            stop("The attributes must be a list")
        }
        attributes$class <- NULL
    }
    
    classType <- getAltClassType(className)
    altBaseClassName <- getAltBaseClassName(classType)
    
    
    if(is.logical(S3Class)&&S3Class){
        attributes$class <- getAltS3ClassVector(classType)
    }
    if(is.character(S3Class)){
        attributes$class <- S3Class
        S3Class <- TRUE
    }
    RS4ClassName <- NULL
    if(is.logical(S4Class)&&S4Class){
        RS4ClassName <- altBaseClassName
    }
    if(is.character(S4Class)){
        RS4ClassName = S4Class
        S4Class = TRUE
    }
    
    if (S3Class && S4Class) {
        stop("The altWrapper object cannot be both S3 and S4 class type")
    }
    
    
    state = list(
        packageName = "AltWrapper",
        className = as.symbol(className),
        classType = classType
    )
    
    ## This unfortunate code is for reducing the reference count
    ## for S3 or atomic object.
    ## For S4 object the reference number is always 7
    ## Hopefully it can be solved in future
    if (S4Class) {
        altWrapperObject <- C_create_altrep(className,
                                           x,
                                           classType,
                                           state,
                                           names(attributes),
                                           attributes)
        new(RS4ClassName, altWrapperObject)
    } else{
        C_create_altrep(className,
                        x,
                        classType,
                        state,
                        names(attributes),
                        attributes)
    }
}


################################
## Set altWrapper class
################################

#' Define an altWrapper class
#'
#' Define an altWrapper class by specifying its name and data type
#'
#' @param className Character, the name of the class
#' @param classType Character, the type of the altWrapper
#'
#' @inherit newAltrep examples
#' @return No return value
#' @export
setAltClass <-
    function(className,
             classType = c("raw", "logical", "integer", "double")) {
        className <- as.character(className)
        classType <- as.character(match.arg(classType))
        
        if (isAltClassDefined(className) &&
            getAltWrapperOptions("redefineWarning")) {
            warning("The class '",
                    className,
                    "' has been defined and will be replaced.")
        }
        
        classSpace <- list()
        classSpace[["classType"]] <- classType
        classSpace[["functionSpace"]] <- vector("list", length(altrepClassFunctionList))
        names(classSpace[["functionSpace"]]) <- altrepClassFunctionList
        classSpace[["classSettings"]] <- altWrapperClassDefaultSettings
        
        setClassSpace(className, classSpace)
        invisible()
    }

#' Define altWrapper methods
#'
#' Define altWrapper methods which are
#' wrappers of ALTREP APIs.
#'
#' @inheritParams setAltClass
#' @param inspect Function, see detail
#' @param getLength Function, see detail
#' @param getDataptr Function, see detail
#' @param getDataptrOrNull Function, see detail
#' @param getElement Function, see detail
#' @param getSubset Function, see detail
#' @param getRegion Function, see detail
#' @param duplicate Function, see detail
#' @param coerce Function, see detail
#' @param serialize Function, see detail
#' @param unserialize Function, see detail
#' @param isSorted Function, see detail
#' @param noNA Function, see detail
#' @param sum Function, see detail
#' @param min Function, see detail
#' @param max Function, see detail
#'
#' @inherit newAltrep examples
#'
#'
#'
#' @details
#' This function defines the behavior of an altWrapper class.
#' The altWrapper methods are fundamentally wrappers of ALTREP APIs.
#' Any call to the ALTREP APIs will be converted
#' to a call of the corresponding altWrapper methods.
#'
#' Belows are the definitions of the altWrapper methods.
#' `x` is the data of the altWrapper object.
#'
#' \tabular{lll}{
#' **Function** \tab **Arguments** \tab  **Return value** \cr
#' `inspect` \tab `x` \tab `A logical value`\cr
#' `getLength` \tab `x` \tab `An integer/numeric value`\cr
#' `getDataptr` \tab `x`, `writeable` \tab `Vector/External pointer`\cr
#' `getDataptrOrNull` \tab `x` \tab `Vector/External pointer/NULL`\cr
#' `getElement` \tab `x`, `i` \tab `An entry of the ALTREP vector`\cr
#' `getSubset` \tab `x`, `i` \tab `A subset of the ALTREP vector`\cr
#' `getRegion` \tab `x`, `start`, `length`, `output` \tab `Length of the true reads`\cr
#' `duplicate` \tab `x`, `deep` \tab `The duplicated object`\cr
#' `coerce` \tab `x`, `type` \tab `The coerced object`\cr
#' `serialize` \tab `x` \tab `Any R object`\cr
#' `unserialize` \tab `R_class`, `state` \tab `Any R object`\cr
#' `isSorted` \tab `x` \tab `An integer value`\cr
#' `noNA` \tab `x` \tab `An integer value`\cr
#' `sum` \tab `x`, `na.rm` \tab `An integer/numeric value`\cr
#' `min` \tab `x`, `na.rm` \tab `An integer/numeric value`\cr
#' `max` \tab `x`, `na.rm` \tab `An integer/numeric value`\cr
#' }
#'
#' `inspect` function determines the output of a `.Internal(inspect(...))` call.
#' Return value = `FALSE` means using the default method to inspect the object.
#'
#' `getLength` function returns the length of the ALTREP object
#'
#' `getDataptr` function returns the data pointer of the ALTREP object.
#' The argument `writeable` means whether R will write data to the pointer.
#' Unlike `getDataptrOrNull` function. This function must return a pointer.
#' It is suggested to throw an error if the ALTREP object does not have a
#' data pointer. Please note that before R 3.7, `getDataptr` is the only way
#' to print out an ALTREP object without using `S3` or `S4` method dispatching
#' when the data pointer of the object is not available.
#'
#' `getDataptrOrNull` function is similar to `getDataptr` function, but it can
#' return `NULL` if the data pointer is not available.
#'
#' `getElement` function returns the value of the ALTREP vector at a given position.
#' The argument `i` is a 1-based index of the vector.
#'
#' `getSubset` function returns a subset of the ALTREP vector.
#' The argument `i` is a 1-based subset.
#'
#' `getRegion` function reads a continuous region of the ALTREP vector.
#' The argument `start` is the 1-based starting position of the reads.
#' `length` determines the length of the reads.
#' Unlike most R functions, the result of the reads should be returned via
#' the argument `output` instead of using `return` function. Users must use
#' `[<-` operator to change the value of the `output` argument. The return value
#' of `getRegion` function is the true number of reads in the `output` argument.
#' In most case the return value is the same as `length`, it will be different only when
#' `start + length - 1` exceeds the length of the ALTREP vector.
#'
#'
#' `duplicate` function duplicates the ALTREP vector. The argument `deep` determines
#' whether it is a deep copy or not. A default duplicate method will be used if the
#' return value is NULL.
#'
#' `coerce` function coerces the ALTREP vector to the other data type. The
#' integer argument `type` is the index of the target type. Please refer to
#' \href{https://cran.r-project.org/doc/manuals/r-release/R-ints.html#SEXPTYPEs}{SEXPTYPEs}
#' to see the meaning of the index.
#'
#' `serialize` function serialize the ALTREP object. The return value is called `state`
#' and will be used in `unserialize` function.
#'
#' `unserialize` function unserialize the ALTREP object. The argument `state` is
#' the return value of `serialize` function. The argument `R_class` is for compatibility
#' only.
#'
#' `isSorted` whether the ALTREP vector is sorted in the ascending or descending order. 
#' The vector status can be unknown sortness, known unsorted, ascending, descending,
#' ascending with NA first, descending with NA first. You can find the corresponding
#' return values by accessing the list variable `altrepSortStatus`.
#' 
#' `noNA` check whether the ALTREP vector contains any NA value.
#'  The return value must be an element of the list `altrepNAStatus`. 
#'
#' `sum`, `min` and `max` have the same meaning as R's corresponding functions. These
#' functions are only available for `integer` and `double` ALTREP class type.
#'
#' @return No return value
#' @export
setAltMethod <- function(className,
                         inspect,
                         getLength,
                         getDataptr,
                         getDataptrOrNull,
                         getElement,
                         getSubset,
                         getRegion,
                         duplicate,
                         coerce,
                         serialize,
                         unserialize,
                         isSorted,
                         noNA,
                         sum,
                         min,
                         max) {
    call <- match.call()
    args <- names(call)[seq_len(length(call) - 2) + 2]
    
    if (!isAltClassDefined(className)) {
        stop("The class '", className, "' is not found.")
    }
    
    for (i in args) {
        func <- get(as.character(i))
        .setAltMethod(className, i, func)
    }
}
