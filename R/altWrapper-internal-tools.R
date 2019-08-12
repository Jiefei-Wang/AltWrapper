## Set the ALTREP method
.setAltMethod <- function(className, functionName, func) {
    if(!is.null(func)){
        argsNum = length(formals(func))
        expectedNum=altrepClassFunctionArgNum[functionName]
        if(argsNum!=expectedNum){
            stop("The number of arguments of the function `",functionName,"` does not match the requirement.\n",
                 "\t Expected: ",expectedNum,"\t Actual: ",argsNum)
        }
    }
    classEnv = altrepRegistryEnvironment[[className]]
    func = .addAltrepArg(func)
    .setMethod(classEnv, functionName, func)
}
## Attach a method to an ALTREP class
## The method will not be called by R
.attachAltMethod <- function(className, functionName, func) {
    classEnv = .getClassFunctionEnvironment(className)
    .setMethod(classEnv, functionName, func)
}

## A general function to assign a function to an environment
## If the function is NULL, it will remove the function from the
## environment
.setMethod <- function(classEnv, functionName, func) {
    if (is.null(func)) {
        rm(list = functionName, envir = classEnv)
        return()
    }
    if ((!is.null(classEnv[[functionName]])) &&
        getAltWrapperOptions("redefineWarning")) {
        warning("The method '",
                functionName,
                "' has been defined and will be replaced.")
    }
    classEnv[[functionName]] = func
}

## Get the class environment
## The class environment is the place where
## all the ALTREP functions and class settings for
## an altWrapper class are stored.
.getClassEnvironment <- function(className) {
    classEnv = altrepRegistryEnvironment[[className]]
    classEnv
}
.setClassEnvironment <- function(className, classEnv) {
    altrepRegistryEnvironment[[className]] = classEnv
}
## Get the class function environment
## It is the sub environment of the class environment
## The environment is used to store the attached functions
.getClassFunctionEnvironment <- function(className) {
    classEnv = .getClassEnvironment(className)
    classEnv[["functionEnvironment"]]
}



## Check if x is an altWrapper
## x must be an altrep
.isAltWrapper <- function(x) {
    data2 = getAltData2(x)
    is.list(data2) &&
        length(data2) > 0 &&
        data2[[1]] == "AltWrapper"
}

## Get Class name by either its name or the altWrapper object
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


## Add an altrep argument(.self) to a function
.addAltrepArg <- function(func) {
    if (is.null(func))
        return(NULL)
    args = formals(func)
    formals(func) <- c(args, alist(.self = ))
    func
}

## remove an altrep argument(.self) from a function
.removeAltrepArg <- function(func) {
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
    if (methodName %in% altrepClassFunctionList) {
        classEnv = .getClassEnvironment(className)
    } else{
        classEnv = .getClassFunctionEnvironment(className)
    }
    if (showInternal) {
        return(classEnv[[methodName]])
    } else{
        return(.removeAltrepArg(classEnv[[methodName]]))
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
    print("Internal serialize altWrapper function")
    # autoExport = getAltClassSetting(className = className,
    #                                 settingName = "autoExportClassDef")
    # serializeObject = list(
    #     autoExport = autoExport,
    #     className = as.symbol(className),
    #     classEnv = NULL,
    #     state = state
    # )
    # 
    # if (autoExport) {
    #     classEnv = .getClassEnvironment(className)
    #     #print(as.list(classEnv))
    #     serializeObject[["classEnv"]] = classEnv
    # }
    # serializeObject
    NULL
}

#' Unserialize altWraper object
#'
#' This function unpack serialized object and register
#' the altWrapper functions.
#'
#' @noRd
.unserializeAltWrapper <- function(serializedInfo) {
    print("Internal unserialize altWrapper function")
    #message(serializedInfo)
    if (serializedInfo[["autoExport"]]) {
        className = as.character(serializedInfo[["className"]])
        classEnv = serializedInfo[["classEnv"]]
        # print(serializedInfo)
        # print(classEnv)
        # print(as.list(classEnv))
        .setClassEnvironment(className, classEnv)
    }
}


.getAltClassSetting <- function(className, settingName) {
    classEnv = .getClassEnvironment(className)
    settingList = classEnv[["classSettings"]]
    settingList[settingName]
}

.setAltClassSetting <- function(className, settingName, value) {
    classEnv = .getClassEnvironment(className)
    settingList = classEnv[["classSettings"]]
    settingList[[settingName]] = value
    classEnv[["classSettings"]] = settingList
}
