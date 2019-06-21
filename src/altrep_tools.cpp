#include "tools.h"
#include "altrep_tools.h"
#include "altrep.h"
#include "altrep_macro.h"

SEXP make_call(SEXP fun) {
	SEXP call = Rf_lang1(fun);
	return R_forceAndCall(call, 0, R_GlobalEnv);
}
SEXP make_call(SEXP fun, SEXP x1) {
	SEXP call = Rf_lang2(fun, x1);
	return R_forceAndCall(call, 1, R_GlobalEnv);
}
SEXP make_call(SEXP fun, SEXP x1, SEXP x2) {
	SEXP call = Rf_lang3(fun, x1, x2);
	return R_forceAndCall(call, 2, R_GlobalEnv);
}
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3) {
	SEXP call = Rf_lang4(fun, x1, x2, x3);
	return R_forceAndCall(call, 3, R_GlobalEnv);
}
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3, SEXP x4) {
	SEXP call = Rf_lang5(fun, x1, x2, x3, x4);
	return R_forceAndCall(call, 4, R_GlobalEnv);
}


void remove_object(SEXP env, SEXP symbol) {
	try {
		Environment package_env(R_FindNamespace(Rf_mkString("base")));
		Function remove = package_env["remove"];

		remove(Named("list")=wrap(SYMBOL_TO_CHAR(symbol)), Named("envir")= env);
	}
	catch (const std::exception& ex) {
		errorHandle("Error in removing an object\n%s", ex.what());
	}

}

SEXP ALTREP_SYMBOL_LIST;

SEXP get_alt_symbol(const char* name) {
#define X(i,func_name) \
if (std::strcmp(name, #func_name)==0)\
	return VECTOR_ELT(ALTREP_SYMBOL_LIST, i);
	ALTREP_FUNCTIONS
#undef X
	unsigned int offset = ALTREP_FUNCTION_NUMBER;
	if (std::strcmp(name, "class_type") == 0)
		return VECTOR_ELT(ALTREP_SYMBOL_LIST, offset+0);
	errorHandle("The symbol '%s' is not found.\n", name);
}

//Initialize the symbol list
void init_altrep_symbol_list() {
#define X(i,func_name) \
SET_VECTOR_ELT(ALTREP_SYMBOL_LIST, i, Rf_install(#func_name));
	ALTREP_FUNCTIONS
#undef X
	unsigned int offset = ALTREP_FUNCTION_NUMBER;
	SET_VECTOR_ELT(ALTREP_SYMBOL_LIST, offset, Rf_install("class_type"));
	//Rf_PrintValue(ALTREP_SYMBOL_LIST);
}


