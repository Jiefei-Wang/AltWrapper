
#define ALTREP_FUNCTION_NUMBER 11

#define ALTREP_FUNCTIONS \
X(0,inspect)\
X(1,length)\
X(2,duplicate)\
X(3,coerce)\
X(4,serialize)\
X(5,unserialize)\
X(6,dataptr)\
X(7,dataptr_or_null)\
X(8,subset)\
X(9,get_element)\
X(10,region)



// x must be a symbol
#define HAS_ENV_VAR(env,x) (Rf_findVarInFrame3(env, x, FALSE)!= R_UnboundValue)

// x must be a symbol
#define HAS_ALT_CLASS(x) HAS_ENV_VAR(ALTREP_CLASS_SPACE,x)
#define GET_ALT_CLASS(x) Rf_findVarInFrame(ALTREP_CLASS_SPACE, x)

// alt is an environment, func must be a symbol
#define HAS_ALT_METHOD(alt,func) HAS_ENV_VAR(alt,func)
#define GET_ALT_METHOD(alt,func) Rf_findVarInFrame(alt,func)

#define GET_ALT_DATA(x) R_altrep_data1(x)
#define GET_ALT_ATTR(x,i) VECTOR_ELT(R_altrep_data2(x),i)
#define SET_ALT_ATTR(x,i,value) SET_VECTOR_ELT(R_altrep_data2(x),i,value)

#define GET_ALT_CLASS_NAME_SYMBOL(x) GET_ALT_ATTR(x,0)
#define GET_ALT_CLASS_TYPE(x) GET_ALT_ATTR(x,1)
#define SET_ALT_CLASS_NAME_SYMBOL(x,value) SET_ALT_ATTR(x,0,value)
#define SET_ALT_CLASS_TYPE(x,value) SET_ALT_ATTR(x,1,value)




#define ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol)\
SEXP alt_class_env =GET_ALT_CLASS(alt_class_name_symbol);\
if(alt_class_env==R_UnboundValue) errorHandle("Unable to find the var '%s' in the class '%s'",SEXP_TO_CHAR(alt_class_func_symbol),SEXP_TO_CHAR(alt_class_name_symbol));\
SEXP func = GET_ALT_METHOD(alt_class_env, alt_class_func_symbol);


#define GET_ALT_SYMBOL(x) get_alt_symbol(#x)