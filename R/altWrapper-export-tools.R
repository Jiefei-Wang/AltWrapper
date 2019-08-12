################################
## Altwrapper internal function
################################
#' Set the data for an altwrapper object in ALTREP functions
#'
#' Set the data for an altwrapper object inside an ALTREP function,
#' The function is only allowed to be called in an ALTREP API.
#' It is designed for caching data for an altWrapper object(eg. sum value,
#' a block of on-disk data).
#' Please refer to `?setAltMethod` to find a full list of the ALTREP APIs.
#'
#'
#' @param x The data of an altWrapper object
#' @examples
#' ## The data is a list
#' data=list(data=runif(10))
#'
#' ## Define the ALTREP functions
#' length_func<-function(x) length(x$data)
#' get_ptr_func<-function(x,writeable) x$data
#'
#' ## Define a sum function,
#' ## after the fisrt computation,
#' ## store the result in the list.
#' ## Call `setAltSelfData` to set the data
#' sum_func<-function(x, na.rm) {
#'    if(is.null(x$sum)){
#'      message("computing sum")
#'      x$sum = sum(x$data)
#'      setAltSelfData(x)
#'      return(x$sum)
#'    }else{
#'      message("reading sum result")
#'      return(x$sum)
#'    }
#' }
#'
#' ## Define the altWrapper class and its functions
#' setAltClass(className = "example", classType = "real")
#' setAltMethod(className = "example", getLength = length_func)
#' setAltMethod(className = "example", getDataptr = get_ptr_func)
#' setAltMethod(className = "example", sum = sum_func)
#'
#' A=makeAltrep(className = "example", x = data)
#'
#' #First sum call
#' sum(A)
#' #Second sum call
#' sum(A)
#' @return no return value
#'
#' @export
setAltSelfData <- function(x) {
    altObject = get(".self", envir = parent.frame())
    altObject = removeWrapper(altObject)
    
    setAltData1(altObject, x)
}



################################
## Altwrapper settings
################################
#' AltWrapper APIs
#'
#' @param className Character, the name of an altWrapper class
#' @param methodName Character, the name of a function
#' @param x an altwrapper object
#' @examples
#' ## Define the ALTREP functions
#' length_func<-function(x) length(x)
#' get_ptr_func<-function(x,writeable) x
#'
#'
#' ## Define the altWrapper class and its functions
#' setAltClass(className = "example", classType = "integer")
#' setAltMethod(className = "example", getLength = length_func)
#' setAltMethod(className = "example", getDataptr = get_ptr_func)
#'
#' ## Create an altWrapper object by providing the class name and data.
#' A=makeAltrep(className = "example", x = 1L:10L)
#' A
#'
#' ## Check the existance of an altWrapper class
#' isAltClassExist(className = "example")
#'
#' ## Check the existance of a function of an altWrapper class
#' isAltMethodExist(className = "example", methodName = "getLength")
#'
#' ## Get a function from an altWrapper class
#' getAltMethod(className = "example", methodName = "getLength")
#'
#' ## Get the class name by an altWrapper object
#' getAltClassName(A)
#'
#' ## Get the class type either by a class name or by an altWrapper object
#' getClassType(className = "example")
#' getClassType(x = A)
#'
#' ## Get the data of an altWrapper object
#' getAltWrapperData(x = A)
#'
#' ## Set the data of an altWrapper object
#' setAltWrapperData(x = A, value = 11L:20L)
#'
#' ## Show the status of an altWrapper class
#' ## The class can be found either by its name or an altWrapper object
#' altClassStatus(className = "example")
#' altClassStatus(x = A)
#'
#' @details
#' `isAltClassExist` : Whether an altWrapper class exist or not
#' @return
#' `isAltClassExist` : Logical
#' @rdname altwrapper-api
#' @export
isAltClassExist <- function(className) {
    className = as.character(className)
    ! is.null(.getClassEnvironment(className))
}

#' @details
#' `isAltMethodExist` : Whether an altWrapper method for
#' an altWrapper class exist or not. This function will return `FALSE`
#' if the altWrapper class does not exist.
#' @rdname altwrapper-api
#' @return
#' `isAltMethodExist` : Logical
#' @export
isAltMethodExist <- function(className, methodName) {
    className = as.character(className)
    methodName = as.character(methodName)
    if (!isAltClassExist(className))
        return(FALSE)
    
    if (methodName %in% altrepClassFunctionList) {
        classEnv = .getClassEnvironment(className)
    } else{
        classEnv = .getClassFunctionEnvironment(className)
    }
    ! is.null(classEnv[[methodName]])
}

#' @details
#' `getAltClassName` : Get the altWrapper class name by
#'  an altWrapper object
#' @rdname altwrapper-api
#' @return
#' `getAltClassName` : A character name
#' @export
getAltClassName <- function(x) {
    if (!is.altrep(x))
        stop("The data is not an ALTREP object.")
    x = removeWrapper(x)
    data2 = getAltData2(x)
    className = as.character(data2[["className"]])
    className
}

#' @details
#' `getClassType` : Get the altWrapper class type by
#' a class name or an altWrapper object
#' @rdname altwrapper-api
#' @return
#' `getClassType` : A character indicating data type
#' @export
getClassType <- function(className = NULL, x = NULL) {
    className = getClassName(className = className, x = x)
    classEnv = .getClassEnvironment(className)
    classEnv[["classType"]]
}

#' @details
#' `getAltMethod` : Get the altWrapper method by
#' a method name. Either `className` or `x` must be specified to
#' find the altWrapper class. The method can be either a wrapper method
#' of ALTREP API or a method attached to the altWrapper class.
#' @rdname altwrapper-api
#' @return
#' `getAltMethod` : An R function or NULL if the function is not found
#' @export
getAltMethod <- function(className = NULL,
                         methodName,
                         x = NULL) {
    if (length(methodName) > 1) {
        return(lapply(methodName, function(method)
            getAltMethod(className=className, methodName=method, x=x)))
    }
    
    .getAltMethod(
        className = className,
        methodName = methodName,
        x = x,
        showInternal = FALSE
    )
}

#' @details
#' `getAltWrapperData` : Get the data for an altwrapper object
#' @rdname altwrapper-api
#' @return
#' `getAltWrapperData` : An R object
#' @export
getAltWrapperData <- function(x) {
    if (!is.altWrapper(x))
        stop("The object is not altWrapper")
    getAltData1(x)
}

#' @param value The data of an altWrapper object
#' @details
#' `setAltWrapperData` : Set the data of an altwrapper object, it
#' might trigger a duplication of the object.
#' @rdname altwrapper-api
#' @return
#' `setAltWrapperData` : The altWrapper object
#' @export
setAltWrapperData <- function(x, value) {
    if (!is.altWrapper(x))
        stop("The object is not altWrapper")
    if (C_getName(x) > 1) {
        return(makeAltrep(
            className = getClassName(x = x),
            x = value,
            attributes = attributes(x)
        ))
    } else{
        setAltData1(x, value)
        return(x)
    }
}



#' @details
#' `deleteClass` : Delete an AltWrapper class
#' @rdname altwrapper-api
#' @return
#' `deleteClass` : No return value
#' @export
deleteClass <- function(className) {
    className = as.character(className)
    if (isAltClassExist(className)) {
        rm(list = className, envir = altrepRegistryEnvironment)
    } else{
        warning("The class '", className, "' is not found")
    }
}

#' @details
#' `altClassStatus` : Show the status of an altWrapper class
#' @rdname altwrapper-api
#' @return
#' `altClassStatus` : No return value
#' @export
altClassStatus <- function(className = NULL, x = NULL) {
    className = getClassName(className = className, x = x)
    classType = getClassType(className, x)
    title = c("Class name", "Class type")
    content = c(className, classType)
    
    classEnv = .getClassEnvironment(className)
    statusChar = c("defined", "undefined")
    for (i in altrepClassFunctionList) {
        if (is.na(i))
            break
        isExist = statusChar[is.null(classEnv[[i]]) + 1]
        title = c(title, paste0(" ", as.character(i)))
        content = c(content, isExist)
    }
    classFuncEnv = .getClassFunctionEnvironment(className)
    
    title = c(title, " Attached Func #")
    content = c(content, length(classFuncEnv))
    
    title = StrAlign(paste0(title, " : "), sep = "\\l")
    
    output = paste0(title, content, "\n")
    cat(output, sep = "")
}

#' @export
getAltClassSetting<-function(className = NULL, 
                             settingName =NULL,
                             x = NULL){
    className = getClassName(className = className, x = x)
    if(is.null(settingName)){
        settingName=names(altWrapperClassDefaultSettings)
    }
    result=.getAltClassSetting(className=className,settingName)
    if(length(result)==1) 
        result=unlist(result)
    result
}
#' @export
setAltClassSetting<-function(className= NULL,
                             ...,
                             x=NULL){
    className = getClassName(className = className, x = x)
    args=c(...)
    argNames=names(args)
    missingInd=which(!argNames%in%names(altWrapperClassDefaultSettings))
    if(length(missingInd)>0){
        warning("Unsupported settings: ",paste0(argNames[missingInd],collapse=", "))
    }
    for(i in argNames){
        .setAltClassSetting(className, i, args[[i]])
    }
    
}

