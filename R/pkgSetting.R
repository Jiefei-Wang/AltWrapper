globalSettings<-new.env()
globalSettings$redefineWarning=TRUE


setAltWrapperOptions<-function(...){
  options=list(...)
  options=checkOptionExistance(options)
  for(i in seq_along(options)){
    globalSettings[[names(options)[i]]]=options[[i]]
  }
}

getAltWrapperOptions<-function(...){
  options=c(...)
  if(length(options)==0){
    return(as.list(globalSettings))
  }
  return(as.list(globalSettings)[options])
}

## Check if options exist or not
## return the options that exist
checkOptionExistance<-function(options){
  noneExistOptions=!names(options)%in%names(globalSettings)
  if (any(noneExistOptions)) {
    vapply(paste0(names(options)[noneExistOptions]), function(x)
      warning("The option `", x, "` does not exist"),character(1))
  }
  options=options[!noneExistOptions]
  return(options)
}