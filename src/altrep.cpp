#include "Rcpp.h"
#include "R_ext/Altrep.h"
#include "altrep.h"
#include "tools.h"
#include "altrep_macro.h"
#include "altrep_tools.h"

using namespace Rcpp;


SEXP get_valid_func_name() {
	SEXP res = PROTECT(Rf_allocVector(VECSXP, ALTREP_FUNCTION_NUMBER));
#define X(i,func_name) \
SET_VECTOR_ELT(res, i, GET_ALT_SYMBOL(func_name));
	ALTREP_FUNCTIONS
#undef X
	UNPROTECT(1);
	return(res);
}


//This macro will be undefined after use
#define MATCH_TYPE(x) \
if (std::strcmp(as<const char*>(type_name),#x)==0) {\
return(x);\
}
altClassType get_altrep_enum_type_by_type_name(SEXP type_name) {
	MATCH_TYPE(logical);
	MATCH_TYPE(integer);
	MATCH_TYPE(real);
	errorHandle("The class type is not invalid\n");
}
#undef MATCH_TYPE

altClassType get_altrep_enum_type_by_class_symbol(SEXP class_symbol_name) {
	SEXP class_env = GET_ALT_CLASS(class_symbol_name);
	return (altClassType)as<int>(Rf_findVarInFrame(class_env, GET_ALT_SYMBOL(class_type)));
}
R_altrep_class_t get_altrep_class(SEXP class_symbol_name) {
	altClassType class_type = get_altrep_enum_type_by_class_symbol(class_symbol_name);
	switch (class_type) {
	case integer:
		return altrep_integer_class;
	case logical:
		errorHandle("The class type is not available\n");
	case real:
		return altrep_real_class;
	}
	return altrep_real_class;

}
bool has_alt_class(SEXP class_symbol_name) {
	return HAS_ALT_CLASS(class_symbol_name);
}
bool has_alt_function(SEXP class_symbol_name,SEXP function_symbol_name) {
	SEXP alt_class_env = GET_ALT_CLASS(class_symbol_name);
	return HAS_ALT_METHOD(alt_class_env, function_symbol_name);
}


void register_alt_class(SEXP class_symbol_name, altClassType class_type, bool redefineWarning) {
	if (HAS_ALT_CLASS(class_symbol_name) && redefineWarning) {
		warningHandle("The class name '%s' has been registered and will be replaced.", SYMBOL_TO_CHAR(class_symbol_name));
	}
	SEXP class_env = PROTECT(Rf_allocSExp(ENVSXP));
	//Register class
	Rf_defineVar(class_symbol_name, class_env, ALTREP_CLASS_SPACE);
	//record the class type
	Rf_defineVar(GET_ALT_SYMBOL(class_type), wrap((int)class_type), class_env);
	UNPROTECT(1);
}

void register_alt_method(SEXP class_symbol_name,SEXP func_symbol_name ,SEXP func,bool redefineWarning) {
	if (!HAS_ALT_CLASS(class_symbol_name)) {
		errorHandle("The ALTREP class '%s' is not found.", SYMBOL_TO_CHAR(class_symbol_name));
	}
	
	SEXP class_env = GET_ALT_CLASS(class_symbol_name);
	//Remove the method when the value is null
	if (TYPEOF(func) == NILSXP) {
		if (HAS_ALT_METHOD(class_env, func_symbol_name)) {
			remove_object(class_env, func_symbol_name);
		}
		return;
	}

	if (HAS_ALT_METHOD(class_env, func_symbol_name) && redefineWarning) {
		warningHandle("The method '%s' has been registered and will be replaced.", 
			SYMBOL_TO_CHAR(func_symbol_name), SYMBOL_TO_CHAR(class_symbol_name));
	}
	SEXP func_dup = PROTECT(Rf_duplicate(func));
	//if (func_dup == func) errorHandle("aaa");
	Rf_defineVar(func_symbol_name, func_dup, class_env);
	UNPROTECT(1);
}

void delete_class(SEXP class_symbol_name) {
	if (HAS_ALT_CLASS(class_symbol_name)) {
		remove_object(ALTREP_CLASS_SPACE, class_symbol_name);
	}
	else {
		errorHandle("The ALTREP class '%s' is not found.", SYMBOL_TO_CHAR(class_symbol_name));
	}
}
