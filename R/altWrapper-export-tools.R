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
## Altwrapper operations
################################
#' @details
#' `is.altWrapper` : Is an object altWrapper?
#' @rdname typeCheck
#' @examples
#'
#' ## An altWrapper object is ALTREP and altWrapper
#' length_func<-function(x) length(x)
#' setAltClass(className = "example", classType = "integer")
#' setAltMethod(className = "example", getLength = length_func)
#' A=makeAltrep(className = "example", x = 1L:10L)
#' is.altrep(x)
#' is.altWrapper(A)
#' @return Logical value indicating whether the object is an altWrapper
#' @export
is.altWrapper <- function(x) {
    if (!is.altrep(x))
        return(FALSE)
    .isAltWrapper(x)
}

#' Remove the wrapper created by R
#'
#' This function can only be used with an altWrapper object. It
#' will remove the wrapper created by R.
#'
#' @param x An altWrapper object
#'
#' @examples
#' ## Define an altWrapper class
#' length_func<-function(x) length(x)
#' setAltClass(className = "example", classType = "integer")
#' setAltMethod(className = "example", getLength = length_func)
#' A=makeAltrep(className = "example", x = 1L:10L)
#'
#' ## Since A is a new object and does not have a wrapper,
#' ## calling `removeWrapper` does not have any effect.
#' A=removeWrapper(A)
#' @return An altWrapper object
#' @export
removeWrapper <- function(x) {
    repeat {
        if (!is.altrep(x))
            stop("The object is not an altWrapper")
        if (.isAltWrapper(x))
            return(x)
        x = getAltData1(x)
    }
    return(x)
}

#' Duplicate an R object
#' 
#' Duplicate an R object, the argument shallow controls whether
#' to duplicate the container only or the entire object. It can
#' be useful in defining the duplication function of an ALTREP.
#' Please note that the function does not work with an enviroment
#' object. It will return the same enviroment object.
#' 
#' @param x The object that will be duplicated. The object should 
#' not be an enviroment object.
#' @param shallow Logical, shallow duplicate or not
#' 
#' @return An object of the same type as the input
#' @examples 
#' a = 10
#' b = duplicateObject(a)
#' .Internal(inspect(a))
#' .Internal(inspect(b))
#' @export
duplicateObject<-function(x, shallow = FALSE){
    C_duplicate(x, shallow)
}


################################
## Altwrapper settings
################################
#' AltWrapper APIs
#'
#' @param className Character, the name of an altWrapper class
#' @param methodName Character, the name of a function
#' @param x An altwrapper object
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
            getAltMethod(
                className = className,
                methodName = method,
                x = x
            )))
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
#' @param warning Logical, whether to give an warning if the class is not found.
#' @rdname altwrapper-api
#' @return
#' `deleteClass` : No return value
#' @export
deleteClass <- function(className, warning = TRUE) {
    className = as.character(className)
    if (isAltClassExist(className)) {
        rm(list = className, envir = altrepRegistryEnvironment)
    } else{
        if (warning) {
            warning("The class '", className, "' is not found")
        }
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

#' Get/Set altClass settings
#'
#' The function get or set altClass settings. The setting include
#' `autoExportClassDef`, `autoDuplicate` and `autoSerialize`.
#'
#' @inheritParams getClassType
#' @param settingName A character vector. The name of the setting you want to query
#' @param ... Named arguments. It is used to change the setting.
#' @details
#' `autoExportClassDef` determines whether the definition of a class will be exported to
#' other processes along with the exported variable(Default `TRUE`). If the setting
#' is FALSE, users are responsible to export the class definition to the other processes before
#' exporting an altWrapper variable in order to make sure the exported variable works properly.
#'
#' `autoDuplicate` determines whether an altWrapper class can use a default duplication method
#' (Default `TRUE`).
#' The default duplication method will copy the underlying data that an altWrapper variable
#' is using to duplicate the altWrapper variable and resulting a new altWrapper object.
#' The default duplication is useful when
#' the data is not a reference of the other data sources. In case that the underlying data
#' is a reference(eg. file handle), the default duplication will fail to duplicate the
#' variable since only the handle will be duplicated. Users should define the
#' duplication function to overwrite the default behavior. An error will be thrown out
#' If `autoDuplicate` is FALSE and no duplication method is provided.
#'
#' `autoSerialize` determines whether an altWrapper class can use a default serialize method
#' (Default `TRUE`). The default serialize method will serialize the underlying data of an
#' altWrapper object and send it to the other processes. If the altWrapper object relys on the
#' other data that is only available in the current processes, users should provide their customized
#' serialize function to overwrite the default serialize function. An error will be thrown out
#' If `autoSerialize` is FALSE and no serialize method is provided.
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
#' ##Get altWrapper class settings by class name
#' getAltClassSetting(className = "example")
#' 
#' ##Get altWrapper class settings by altWrapper object
#' getAltClassSetting(x = A)
#' 
#' @rdname altWrapper-setting
#' @return The settings of an altWrapper class
#' @export
getAltClassSetting <- function(className = NULL,
                               settingName = NULL,
                               x = NULL) {
    className = getClassName(className = className, x = x)
    if (is.null(settingName)) {
        settingName = names(altWrapperClassDefaultSettings)
    }
    result = .getAltClassSetting(className = className, settingName)
    if (length(result) == 1)
        result = unlist(result)
    result
}
#' @rdname altWrapper-setting
#' @export
setAltClassSetting <- function(className = NULL,
                               ...,
                               x = NULL) {
    className = getClassName(className = className, x = x)
    args = c(...)
    argNames = names(args)
    missingInd = which(!argNames %in% names(altWrapperClassDefaultSettings))
    if (length(missingInd) > 0) {
        warning("Unsupported settings: ",
                paste0(argNames[missingInd], collapse = ", "))
    }
    for (i in argNames) {
        .setAltClassSetting(className, i, args[[i]])
    }
    
}
