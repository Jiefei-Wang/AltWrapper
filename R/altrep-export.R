altrepRegistryEnvironment = new.env()
altrepClassFunctionList = c(
  "inspect",
  "getLength",
  "getDataptr",
  "getDataptrOrNull",
  "getSubset",
  "getElement",
  "getRegion",
  "duplicate",
  "coerce",
  "serialize",
  "unserialize",
  "isSorted",
  "noNA",
  "sum",
  "min",
  "max"
)

length(altrepClassFunctionList) = 40
altrepSymbolList = c(altrepClassFunctionList,
                     "classType", "functionEnvironment")
altrepSymbolList = lapply(altrepSymbolList, as.symbol)


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
#' @param attributes Named list, attributes that will be attached to the object
#' @param S3Class Logical, whether the return value is of an AltWrapper S3 class. If the
#' argument `attributes` also contains `class` entry, AltWrapper will be a subclass of the class
#' specified in the argument `attributes`.
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
#'
#' ## Create an altWrapper object by providing the class name and data.
#' A=makeAltrep(className = "example", x = 1L:10L)
#' A
#'
#'
#' ## Define a function to compute variance for the altWrapper object.
#' var_func <- function(x, y = NULL, na.rm = FALSE, use)
#'   return(var(x, y, na.rm, use))
#' 
#' ## attach the function to the altWrapper class
#' ## The function cannot be called automatically by R
#' ## Users can retrieve the function via `getAltMethod`
#' ## and call it explicitly.
#' attachAltMethod(className = "example", var = var_func)
#' 
#' @return An ALTREP vector
#' @export
makeAltrep <- function(className,
                       x,
                       attributes = NULL,
                       S3Class = FALSE) {
  if (!isAltClassExist(className)) {
    stop("The class '", className, "' is not found.")
  }
  if (!is.null(attributes) && !is.list(attributes))
    stop("The attributes must be a list")
  if (S3Class) {
    if (is.null(attributes))
      attributes = list()
    attributes$class = c(attributes$class, "altWrapper")
  }
  
  
  classType = getClassType(className)
  state = list(
    packageName = "AltWrapper",
    className = as.symbol(className),
    classType = classType
  )
  
  C_create_altrep(className, x, classType, state, names(attributes), attributes)
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
#' @inherit makeAltrep examples
#' @return No return value
#' @export
setAltClass <-
  function(className,
           classType = c("raw", "logical", "integer", "real")) {
    className = as.character(className)
    classType = as.character(match.arg(classType))
    
    if (isAltClassExist(className) &&
        getAltWrapperOptions("redefineWarning")) {
      warning("The class '",
              className,
              "' has been defined and will be replaced.")
    }
    
    classEnv = new.env()
    classEnv[["classType"]] = classType
    classEnv[["functionEnvironment"]] = new.env()
    
    .setClassEnvironment(className, classEnv)
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
#' @inherit makeAltrep examples
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
#' data pointer.
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
#' `isSorted` and `noNA` check whether the ALTREP vector is sorted and contains any
#' NA value respectively. The return value is an Integer where 0 mean `FALSE` and 1
#' means `TRUE`.
#'
#' `sum`, `min` and `max` have the same meaning as R's corresponding functions. These
#' functions are only available for `integer` and `real` ALTREP class type.
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
  call = match.call()
  args = names(call)[seq_len(length(call) - 2) + 2]
  
  if (!isAltClassExist(className)) {
    stop("The class '", className, "' is not found.")
  }
  
  for (i in args) {
    func = get(as.character(i))
    .setAltMethod(className, i, func)
  }
}

#' Attach ALTREP method
#'
#' Attach a function to an altwrapper class. The function will
#' not be called by R automatically. It is designed to mimic
#' the object-oriented feature for an altwrapper class.
#'
#' @inheritParams makeAltrep
#' @param ... Named arguments. The functions that you want to attach to the class.
#' @inherit makeAltrep examples
#'
#' @return No return value
#' @export
attachAltMethod <- function(className, ...) {
  call = match.call()
  args = names(call)[seq_len(length(call) - 2) + 2]
  for (i in args) {
    .attachAltMethod(className, i, eval(call[[i]]))
  }
}

################################
##ALTREP internal function
################################
#' Set the data for an altwrapper object in ALTREP functions
#'
#' Set the data for an altwrapper object,
#' The function is only allowed to be called in an ALTREP API.
#' Please refer to `?setAltMethod` to find a full list of the ALTREP APIs.
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
##altwrapper settings
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
  className = getClassNameDispatcher(className = className, x = x)
  if (!isAltClassExist(className)) {
    stop("The class '", className, "' is not found.")
  }
  classEnv = .getClassEnvironment(className)
  classEnv[["classType"]]
}
#' @details
#' `getAltMethod` : Get the altWrapper method by
#' a method name. Either `className` or `x` must be specified to
#' find the altWrapper class. The method can be a wrapper method of ALTREP API
#' or a method attached to the altWrapper class.
#' @rdname altwrapper-api
#' @return 
#' `getAltMethod` : An R function or NULL if the function is not found
#' @export
getAltMethod <- function(className = NULL,
                         methodName,
                         x = NULL) {
  if (length(methodName) > 1) {
    return(lapply(methodName, function(y)
      getAltMethod(className, x, y)))
  }
  className = getClassNameDispatcher(className = className, x = x)
  if (!isAltClassExist(className)) {
    NULL
  }
  
  if (methodName %in% altrepClassFunctionList) {
    classEnv = .getClassEnvironment(className)
  } else{
    classEnv = .getClassFunctionEnvironment(className)
  }
  return(classEnv[[methodName]])
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
#' `setAltWrapperData` : No return value
#' @export
setAltWrapperData <- function(x, value) {
  if (!is.altWrapper(x))
    stop("The object is not altWrapper")
  if(C_getName(x)>1){
    x=C_duplicate(x,shallow=TRUE)
  }
  setAltData1(x, value)
  x
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
  className = getClassNameDispatcher(className = className, x = x)
  classType = getClassType(className, x)
  title = c("Class name", "Class type")
  content = c(className, classType)
  
  if (!isAltClassExist(className)) {
    stop("The class '", className, "' is not found.")
  }
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
  if(!is.altWrapper(x))
    stop("The object is not created by AltWrapper package")
  x = removeWrapper(x)
  xData = getAltData1(x)
  xLength = length(x)
  className = getAltClassName(x)
  classType = getClassType(className)
  ## If the data pointer is accessable, use default method
  func = getAltMethod(className = className, methodName = "getDataptrOrNull")
  if (!is.null(func)&&!is.null(func(xData,x))) {
    print.default(x)
    return(invisible())
  }
  
  ## Chunk settings
  maxPrint = getOption("max.print")
  printSize = min(maxPrint, xLength)
  chunkSize = 512
  chunkNum = ceiling(printSize / chunkSize)
  
  ## Get data from getRegion
  func = getAltMethod(className = className, methodName = "getRegion")
  if (!is.null(func)) {
    regionVector = C_create_internal_altrep(classType,chunkSize)
    output = get(classType)(printSize)
    for (i in seq_len(chunkNum)) {
      start = (i - 1) * chunkSize
      len = func(xData, start + 1, chunkSize, regionVector, x)
      output[start + seq_len(len)] = regionVector[seq_len(len)]
    }
    print(output)
    return(invisible())
  }
  
  ## Get data from getSubset
  func = getAltMethod(className = className, methodName = "getSubset")
  if (!is.null(func)) {
    output = get(classType)(printSize)
    for (i in seq_len(chunkNum)) {
      start = (i - 1) * chunkSize
      len = min(chunkSize, xLength - start)
      regionVector = func(xData, start + seq_len(len), x)
      output[start + seq_len(len)] = regionVector
    }
    print(output)
    return(invisible())
  }
  
  ## get Data from getElement
  func = getAltMethod(className = className, methodName = "getElement")
  if (!is.null(func)) {
    output = get(classType)(printSize)
    for (i in seq_len(printSize)) {
      output[i] = func(xData, i, x)
    }
    print(output)
    return(invisible())
  }
  stop("The data is not available")
}


#' @rdname S3Print
#' @export
printAltWrapper<-function(x,...){
  print.altWrapper(x,...)
}

