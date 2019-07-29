

.setAltMethod<-function(className,functionName,func){
  classEnv=altrepRegistryEnvironment[[className]]
  func=addAltrepArg(func)
  .setMethod(classEnv,functionName,func)
}
.attachAltMethod<-function(className,functionName,func){
  classEnv=.getClassFunctionEnvironment(className)
  .setMethod(classEnv,functionName,func)
}




.setMethod<-function(classEnv,functionName,func){
  if(is.null(func)){
    rm(list=functionName,envir=classEnv)
    return()
  }
  if((!is.null(classEnv[[functionName]]))&&getAltWrapperOptions("redefineWarning")){
    warning("The method '",functionName,"' has been defined and will be replaced.")
  }
  classEnv[[functionName]]=func
}

.getClassEnvironment<-function(className){
  classEnv=altrepRegistryEnvironment[[className]]
  classEnv
}
.setClassEnvironment<-function(className,classEnv){
  altrepRegistryEnvironment[[className]]=classEnv
}

.getClassFunctionEnvironment<-function(className){
  classEnv=.getClassEnvironment(className)
  classEnv[["functionEnvironment"]]
}



##Check if x is an altWrapper
##x must be an altrep
.isAltWrapper<-function(x){
  data2=getAltData2(x)
  is.list(data2)&&
       length(data2)>0&&
       data2[[1]]=="AltWrapper"
}


getClassNameDispatcher<-function(className=NULL,x=NULL){
  if(!is.null(x)){
    className=getAltClassName(x)
    return(className)
  }
   if(!is.null(className)){
     if(!is.character(className))
      className = as.character(className)
     return(className)
   } 
  stop("Either class name or data must be specified")
}


#add an altrep argument(.self) to a function
addAltrepArg<-function(func){
  if(is.null(func)) return(NULL)
  args=formals(func)
  formals(func)<-c(args,alist(.self=))
  func
}
removeAltrepArg<-function(func){
  if(is.null(func)) return(NULL)
  args=formals(func)
  argName=names(args)
  if(argName[length(argName)]==".self"){
    formals(func)<-args[-length(length(argName))]
  }
  func
}

.serializeAltWrapper<-function(className,state){
  classEnv=.getClassEnvironment(className)
  list(className=as.symbol(className),classEnv=classEnv,state=state)
}

.unserializeAltWrapper<-function(serializedInfo){
  className=as.character(serializedInfo[["className"]])
  classEnv=serializedInfo[["classEnv"]]
  .setClassEnvironment(className,classEnv)
}

