#' Check the type of an object
#' 
#' @param x An object
#' @details 
#' `is.altrep` : Is an object ALTREP?
#' @rdname typeCheck
#' @examples
#' ## An R vector is not ALTREP nor altWrapper.
#' x=runif(10)
#' is.altrep(x)
#' is.altWrapper(x)
#' 
#' ## A compact sequence is ALTREP but not altWrapper.
#' x=1:10
#' is.altrep(x)
#' is.altWrapper(x)
#' @return Logical value indicating whether the object is an ALTREP
#' @export
is.altrep<-function(x){
  C_ALTREP(x)
}


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
is.altWrapper<-function(x){
  if(!is.altrep(x)) return(FALSE)
  .isAltWrapper(x)
}

#' ALTREP API
#' 
#' Get / Set data1 or data2 from an ALTREP object. 
#' The function will return `NULL` if the object is
#' not an ALTREP. These are native ALTREP APIs, it is
#' not recommended to call these functions to modify an
#' ALTREP object.
#' 
#' @param x An ALTREP object
#' @param value The data that will be set to an ALTREP object
#' @examples 
#' ## A compact sequence is an ALTREP, 
#' ## so ALTREP API can get the data that supports the compact sequence.
#' ## The data is for developer only and not supposed to be used by users.
#' x=1:10
#' getAltData1(x)
#' getAltData2(x)
#' 
#' @return 
#' `getAltData1` : An R object in data1 slot of the ALTREP
#' @rdname altrep-api
#' @export
getAltData1<-function(x){
  if(!is.altrep(x)) return(NULL)
  C_get_alt_data1(x)
}

#' @return 
#' `getAltData2` : An R object in data2 slot of the ALTREP
#' 
#' @rdname altrep-api
#' @export
getAltData2<-function(x){
  if(!is.altrep(x)) return(NULL)
  C_get_alt_data2(x)
}

#' @return 
#' `setAltData1` : `TRUE` if the value has been set or 
#' `FALSE` if the object is not an ALTREP
#' @rdname altrep-api
#' @export
setAltData1<-function(x,value){
  if(!is.altrep(x)) return(FALSE)
  C_set_alt_data1(x,value)
  invisible(TRUE)
}
#' @return 
#' `setAltData2` : `TRUE` if the value has been set or 
#' `FALSE` if the object is not an ALTREP
#' @rdname altrep-api
#' @export
setAltData2<-function(x,value){
  if(!is.altrep(x)) return(FALSE)
  C_set_alt_data2(x,value)
  invisible(TRUE)
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
removeWrapper<-function(x){
  repeat{
    if(!is.altrep(x)) stop("The object is not an altWrapper")
    if(.isAltWrapper(x)) return(x)
    x=getAltData1(x)
  }
  return(x)
}




