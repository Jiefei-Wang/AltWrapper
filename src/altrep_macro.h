
#define ALTREP_FUNCTION_NUMBER 11

#define ALTREP_SYMBOLS \
X(0,inspect)\
X(1,getLength)\
X(2,getDataptr)\
X(3,getDataptrOrNull)\
X(4,getSubset)\
X(5,getElement)\
X(6,getRegion)\
X(7,duplicate)\
X(8,coerce)\
X(9,serialize)\
X(10,unserialize)\
X(11,isSorted)\
X(12,noNA)\
X(13,sum)\
X(14,min)\
X(15,max)\
X(40, classType)\
X(41, functionEnvironment)



// x must be a symbol
#define HAS_ENV_VAR(env,x) (Rf_findVarInFrame3(env, x, FALSE)!= R_UnboundValue)

// x must be a symbol
#define HAS_ALT_CLASS(x) HAS_ENV_VAR(ALTREP_REGISTRY_ENVIRONMENT,x)
#define GET_ALT_CLASS(x) Rf_findVarInFrame(ALTREP_REGISTRY_ENVIRONMENT, x)

// alt is an environment, func must be a symbol
#define HAS_ALT_METHOD(alt,func) HAS_ENV_VAR(alt,func)
#define GET_ALT_METHOD(alt,func) Rf_findVarInFrame(alt,func)

#define GET_ALT_DATA(x) R_altrep_data1(x)
#define GET_ALT_ATTR(x,i) VECTOR_ELT(R_altrep_data2(x),i)
#define SET_ALT_ATTR(x,i,value) SET_VECTOR_ELT(R_altrep_data2(x),i,value)

//This need to be refined
#define GET_ALT_CLASS_NAME_SYMBOL(x) GET_ALT_ATTR(x,1)
#define GET_ALT_CLASS_TYPE(x) GET_ALT_ATTR(x,2)

#define SET_ALT_CLASS_NAME_SYMBOL(x,value) SET_ALT_ATTR(x,1,value)
#define SET_ALT_CLASS_TYPE(x,value) SET_ALT_ATTR(x,2,value)




#define ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol)\
SEXP alt_class_env =GET_ALT_CLASS(alt_class_name_symbol);\
if(alt_class_env==R_UnboundValue) errorHandle("Unable to find the var '%s' in the class '%s'",SYMBOL_TO_CHAR(alt_class_func_symbol),SYMBOL_TO_CHAR(alt_class_name_symbol));\
SEXP func = GET_ALT_METHOD(alt_class_env, alt_class_func_symbol);


#define GET_ALT_SYMBOL(x) get_alt_symbol(#x)