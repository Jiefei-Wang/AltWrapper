## Set the ALTREP method
.setAltMethod <- function(className, functionName, func) {
    if (is.null(func)) {
        classFunctionSpace = getClassFunctionSpace(className)
        classFunctionSpace[functionName] = list(NULL)
        setClassFunctionSpace(className, classFunctionSpace)
        return()
    }
    ## If the function is not null
    argsNum = length(formals(func))
    expectedNum = altrepClassFunctionArgNum[functionName]
    if (argsNum != expectedNum) {
        stop(
            "The number of arguments of the function `",
            functionName,
            "` does not match the requirement.\n",
            "\t Expected: ",
            expectedNum,
            "\t Actual: ",
            argsNum
        )
    }
    func = addAltrepArg(func)
    classFunctionSpace = getClassFunctionSpace(className)
    if ((!is.null(classFunctionSpace[[functionName]])) &&
        getAltWrapperOptions("redefineWarning")) {
        warning("The method '",
                functionName,
                "' has been defined and will be replaced.")
    }
    classFunctionSpace[[functionName]] = func
    setClassFunctionSpace(className, classFunctionSpace)
}

## Get the class environment
## The class environment is the place where
## all the ALTREP functions and class settings for
## an altWrapper class are stored.
getClassSpace <- function(className) {
    classSpace = altrepRegistryEnvironment[[className]]
    classSpace
}
setClassSpace <- function(className, classSpace) {
    altrepRegistryEnvironment[[className]] = classSpace
}
## Get the class function environment
## It is the sub environment of the class environment
## The environment is used to store the attached functions
getClassFunctionSpace <- function(className) {
    classSpace = getClassSpace(className)
    classSpace[["functionSpace"]]
}
setClassFunctionSpace <- function(className, classFunctionSpace) {
    altrepRegistryEnvironment[[className]][["functionSpace"]] = classFunctionSpace
}


## Check if x is an altWrapper
## x must be an altrep
.isAltWrapper <- function(x) {
    data2 = .getAltData2(x)
    is.list(data2) &&
        length(data2) > 0 &&
        data2[[1]] == "AltWrapper"
}

## Get Class name by either its name or the altWrapper object
## verify: whether check if the class has been defined
getClassName <-
    function(className = NULL,
             x = NULL,
             verify = TRUE) {
        if (!is.null(x)) {
            className = getAltClassName(x)
            if (verify && !isAltClassExist(className)) {
                stop("The class '", className, "' is not found.")
            }
            return(className)
        }
        if (!is.null(className)) {
            if (!is.character(className))
                className = as.character(className)
            if (verify && !isAltClassExist(className)) {
                stop("The class '", className, "' is not found.")
            }
            return(className)
        }
        stop("Either class name or data must be specified")
    }


## Convert classType to a base R type
toBaseRType <- function(classType) {
    if (classType == "real")
        classType = "numeric"
    classType
}

## Get the alt class name from class type
## altRaw, altLogical, altInteger, altReal
getAltBaseClassName <- function(classType) {
    ## Capitalize the first letter
    classType = paste0(toupper(substr(classType, 1, 1)), substring(classType, 2))
    paste0("alt", classType)
}

## Add an altrep argument(.self) to a function
addAltrepArg <- function(func) {
    if (is.null(func))
        return(NULL)
    args = formals(func)
    formals(func) <- c(args, alist(.self =))
    func
}

## remove an altrep argument(.self) from a function
removeAltrepArg <- function(func) {
    if (is.null(func))
        return(NULL)
    args = formals(func)
    argName = names(args)
    ## in case that the function is primitive
    ## The argName can be NULL
    if (is.vector(argName) &&
        argName[length(argName)] == ".self") {
        formals(func) <- args[-length(argName)]
    }
    func
}

.getAltMethod <- function(className = NULL,
                          methodName,
                          x = NULL,
                          showInternal = TRUE) {
    className = getClassName(className = className,
                             x = x,
                             verify = FALSE)
    if (!isAltClassExist(className)) {
        NULL
    }
    classSpace = getClassFunctionSpace(className)
    if (showInternal) {
        return(classSpace[[methodName]])
    } else{
        return(removeAltrepArg(classSpace[[methodName]]))
    }
}



#' Serialize altWraper object
#'
#' This function will attach all altWrapper functions to
#' the serialized object to make sure the altWrapper functions
#' in other processes can also be available
#'
#' @noRd
.serializeAltWrapper <- function(className, state) {
    #print("Internal serialize altWrapper function")
    autoExport = getAltClassSetting(className = className,
                                    settingName = "autoExportClassDef")
    serializeObject = list(
        autoExport = autoExport,
        className = as.symbol(className),
        classSpace = NULL,
        state = state
    )
    
    if (autoExport) {
        classSpace = getClassSpace(className)
        serializeObject[["classSpace"]] = classSpace
    }
    serializeObject
}

#' Unserialize altWraper object
#'
#' This function unpack serialized object and register
#' the altWrapper functions.
#'
#' @noRd
.unserializeAltWrapper <- function(serializedInfo) {
    #print(serializedInfo)
    if (serializedInfo[["autoExport"]]) {
        className = as.character(serializedInfo[["className"]])
        classSpace = serializedInfo[["classSpace"]]
        setClassSpace(className, classSpace)
    }
}


.getAltClassSetting <- function(className, settingName) {
    classSpace = getClassSpace(className)
    settingList = classSpace[["classSettings"]]
    settingList[settingName]
}

.setAltClassSetting <- function(className, settingName, value) {
    classSpace = getClassSpace(className)
    settingList = classSpace[["classSettings"]]
    settingList[[settingName]] = value
    classSpace[["classSettings"]] = settingList
    setClassSpace(className, classSpace)
}
