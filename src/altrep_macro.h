/* 
Organization of classSpace:
	classSpace = list()
	classSpace[["classType"]] = classType
	classSpace[["functionSpace"]] = vector("list", length(altrepClassFunctionArgNum))
	classSpace[["classSettings"]] = altWrapperClassDefaultSettings
*/
#define CLASS_TYPE_INDEX 0L
#define FUNCTION_SPACE_INDEX 1L
#define CLASS_SETTINGS_INDEX 2L

//Find the class space from ALTREP_REGISTRY_ENVIRONMENT

// x must be a symbol
#define HAS_ENV_VAR(env,x) (Rf_findVarInFrame3(env, x, FALSE)!= R_UnboundValue)
// class_name must be a symbol
#define HAS_ALT_CLASS(class_name) HAS_ENV_VAR(ALTREP_REGISTRY_ENVIRONMENT,class_name)
#define GET_ALT_CLASS(class_name) Rf_findVarInFrame(ALTREP_REGISTRY_ENVIRONMENT, class_name)
#define GET_ALT_CLASS_TYPE_BY_NAME(class_name) VECTOR_ELT(GET_ALT_CLASS(class_name), CLASS_TYPE_INDEX)


//Find the class subspace by class space
#define GET_ALT_FUNC_SPACE(class_space) VECTOR_ELT(class_space, FUNCTION_SPACE_INDEX)
// class_space is a list, function_name is the function name(no quotation)
#define HAS_ALT_METHOD(class_space,function_name) (GET_ALT_METHOD(class_space,function_name) != R_NilValue)
#define GET_ALT_METHOD(class_space,function_name) VECTOR_ELT(GET_ALT_FUNC_SPACE(class_space), function_name##_index)




//x is an altrep
/*
state = list(
		packageName = "AltWrapper",
		className = as.symbol(className),
		classType = classType
	)
*/
#define GET_ALT_DATA(x) R_altrep_data1(x)
#define GET_ALT_ATTR(x,i) VECTOR_ELT(R_altrep_data2(x),i)
#define SET_ALT_ATTR(x,i,value) SET_VECTOR_ELT(R_altrep_data2(x),i,value)

//Get attributes of an ALTREP(data2 slot)
#define GET_ALT_CLASS_NAME_SYMBOL(x) GET_ALT_ATTR(x,1)
#define GET_ALT_CLASS_TYPE(x) GET_ALT_ATTR(x,2)

#define SET_ALT_CLASS_NAME_SYMBOL(x,value) SET_ALT_ATTR(x,1,value)
#define SET_ALT_CLASS_TYPE(x,value) SET_ALT_ATTR(x,2,value)

//Get class settings of an altWrapper class
//class_name must be a symbol
#define GET_ALT_CLASS_SETTING(class_name) VECTOR_ELT(GET_ALT_CLASS(class_name), CLASS_SETTINGS_INDEX)

#define GET_ALT_CLASS_SETTING_EXPORT_DEF(class_name) as<bool>(VECTOR_ELT(GET_ALT_CLASS_SETTING(class_name),0))
#define GET_ALT_CLASS_SETTING_DUPLICATE(class_name) as<bool>(VECTOR_ELT(GET_ALT_CLASS_SETTING(class_name),1))
#define GET_ALT_CLASS_SETTING_SERIALIZE(class_name) as<bool>(VECTOR_ELT(GET_ALT_CLASS_SETTING(class_name),2))


// Get a function from the ALTREP class specified by the class name
// If the class is not defined, throw an error.
// The existance of the function will not be checked
#define ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_name)\
SEXP alt_class_space =GET_ALT_CLASS(alt_class_name_symbol);\
if(alt_class_space==R_UnboundValue) errorHandle("Unable to find the function '%s' in the class '%s': Class does not exist",\
												#alt_class_func_name ,SYMBOL_TO_CHAR(alt_class_name_symbol));\
SEXP func = GET_ALT_METHOD(alt_class_space, alt_class_func_name);

