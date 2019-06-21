altrep_class_space=new.env()
altrep_symbol_space=vector("list",50)




altrep<-function(className,data){
  className=as.symbol(className)
  C_create_altrep(className,data)
}


setAltClass<-function(className,type){
  className=as.symbol(className)
  type=as.character(type)
  C_set_altrep_class(className,type,globalSettings$redefineWarning)
}

setAltMethod<-function(className,inspect,
                       length,
                       dataptr,dataptrOrNull,
                       getElement,subset,
                       duplicate,coerce,
                       serialize,unserialize,
                       region,...){
  call=match.call()
  args=names(call)
  args[args%in%"dataptrOrNull"]="dataptr_or_null"
  args[args%in%"getElement"]="get_element"
  names(call)=args
  
  if(!"className"%in%args){
    stop("Class name has to be specified!")
  }
  
  matched_func=args[args%in%validFuncList]
  for(i in matched_func){
    .setAltMethod(className,i,call[[i]])
  }
}


.setAltMethod<-function(className,functionName,func){
  className=as.symbol(className)
  functionName=as.symbol(functionName)
  if(all(validFuncList!=functionName)){
    stop("Invalid function name: ",functionName)
  }
  C_set_alt_method(className,functionName,func,globalSettings$redefineWarning)
}


altClassStatus<-function(ClassName){
  ClassName=as.character(ClassName)
  class_env=altrep_class_space[[ClassName]]
  if(is.null(class_env)){
    stop("The class '",ClassName,"' is not found.")
  }
  cat("Class name:\t",ClassName,"\n")
  for(i in validFuncList){
    isExist=ifelse(is.null(class_env[[as.character(i)]]),"undefined","defined")
    cat("",as.character(i),":\t",isExist,"\n")
  }
  
}


