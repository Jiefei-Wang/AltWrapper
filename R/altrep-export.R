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
  "unserialize"
)

length(altrepClassFunctionList) = 40
altrepSymbolList = c(altrepClassFunctionList,
                     "classType", "functionEnvironment")
altrepSymbolList = lapply(altrepSymbolList, as.symbol)


################################
##Mian function
################################

makeAltrep<-function(className,x){
  if(!isAltClassExist(className)){
    stop("The class '",className,"' is not found.")
  }
  
  classType=getClassType(className)
  state=list(packageName="AltWrapper",
             className=as.symbol(className),
             classType=classType)
  
  C_create_altrep(className,x,classType,state)
}


################################
## Set ALTREP class
################################


setAltClass <- function(className, classType=c("raw","logical","integer","real")) {
  className = as.character(className)
  classType=as.character(match.arg(classType))
  
  if (isAltClassExist(className) &&
      getAltWrapperOptions("redefineWarning")) {
    warning("The class '",
            className,
            "' has been define and will be replaced.")
  }
  
  classEnv = new.env()
  classEnv[["classType"]] = classType
  classEnv[["functionEnvironment"]] = new.env()
  
  .setClassEnvironment(className, classEnv)
  invisible()
}

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
                         unserialize) {
  call = match.call()
  args = names(call)[seq_len(length(call)-2)+2]

  if (!isAltClassExist(className)) {
    stop("The class '", className, "' is not found.")
  }

  for (i in args) {
    func=get(as.character(i))
    .setAltMethod(className, i, func)
  }
}

setAltS3Method <- function(className, ...) {
  call = match.call()
  args = names(call)[seq_len(length(call)-2)+2]
  for (i in args) {
    .setS3Method(className, i, eval(call[[i]]))
  }
}
################################
##ALTREP internal function
################################
setAltData<-function(x){
  altObject=get(".self",envir = parent.frame())
  altObject=removeWrapper(altObject)
  setAltData1(altObject,x)
}



################################
##ALTREP settings
################################
isAltClassExist <- function(className) {
  className = as.character(className)
  !is.null(.getClassEnvironment(className))
}

isAltMethodExist <- function(className, methodName) {
  className = as.character(className)
  methodName = as.character(methodName)
  if(!isAltClassExist(className)) return(FALSE)
  
  if(methodName%in%altrepClassFunctionList){
    classEnv = .getClassEnvironment(className)
  }else{
    classEnv = .getClassFunctionEnvironment(className)
  }
  !is.null(classEnv[[methodName]])
}

getAltClassName<-function(x){
  if(!is.altrep(x))
    stop("The data is not an ALTREP object.")
  x=removeWrapper(x)
  data2=getAltData2(x)
  className=as.character(data2[["className"]])
  className
}

getClassType<-function(className=NULL,x=NULL){
  className=getClassNameDispatcher(className=className,x=x)
  if (!isAltClassExist(className)) {
    stop("The class '", className, "' is not found.")
  }
  classEnv=.getClassEnvironment(className)
  classEnv[["classType"]]
}

getAltMethod<-function(className=NULL,x=NULL,methodName){
  if(length(methodName)>1){
    return(sapply(methodName,function(y)getAltFunction(className,x,y)))
  }
  className=getClassNameDispatcher(className=className,x=x)
  if (!isAltClassExist(className)) {
    NULL
  }
  
  if(methodName%in%altrepClassFunctionList){
    classEnv = .getClassEnvironment(className)
  }else{
    classEnv = .getClassFunctionEnvironment(className)
  }
  return(classEnv[[methodName]])
}

removeClass <- function(className) {
  className = as.character(className)
  if (isAltClassExist(className)) {
    rm(list = className, envir = altrepRegistryEnvironment)
  } else{
    warning("The class '", className, "' is not found")
  }
}


altClassStatus <- function(className=NULL,x=NULL) {
  className=getClassNameDispatcher(className=className,x=x)
  classType=getClassType(className,x)
  if (!isAltClassExist(className)) {
    stop("The class '", className, "' is not found.")
  }
  classEnv = .getClassEnvironment(className)
  cat("Class name :\t", className, "\n")
  cat("","data type :\t", classType, "\n")
  statusChar = c("defined", "undefined")
  for (i in altrepClassFunctionList) {
    if(is.na(i)) break
    isExist = statusChar[is.null(classEnv[[i]]) + 1]
    cat("", as.character(i), ":\t", isExist, "\n")
  }
}
