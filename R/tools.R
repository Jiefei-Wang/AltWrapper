is.altrep<-function(x){
  C_ALTREP(x)
}

getAltData1<-function(x){
  C_get_alt_data1(x)
}
getAltData2<-function(x){
  C_get_alt_data2(x)
}
setAltData1<-function(x,value){
  C_set_alt_data1(x,value)
}
setAltData2<-function(x,value){
  C_set_alt_data2(x,value)
}



removeWrapper<-function(x){
  repeat{
    if(!is.altrep(x)) stop("The object is not an altWrapper")
    if(.isAltWrapper(x)) return(x)
    x=getAltData1(x)
  }
  return(x)
}