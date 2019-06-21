#include <Rcpp.h>
#include "altrep.h"
#include "tools.h"
#include <string>
using namespace Rcpp;






// [[Rcpp::export]]
bool C_ALTREP(SEXP x) {
	return ALTREP(x);
}
// [[Rcpp::export]]
SEXP C_get_alt_data1(SEXP x) {
	return R_altrep_data1(x);
}
// [[Rcpp::export]]
SEXP C_get_alt_data2(SEXP x) {
	return R_altrep_data2(x);
}
// [[Rcpp::export]]
void C_set_alt_data1(SEXP x,SEXP value) {
	R_set_altrep_data1(x, value);
}
// [[Rcpp::export]]
void C_set_alt_data2(SEXP x, SEXP value) {
	R_set_altrep_data2(x, value);
}



// [[Rcpp::export]]
SEXP C_duplicate_object(SEXP x, SEXP shallow) {
	if (as<bool>(shallow)) {
		return Rf_shallow_duplicate(x);
	}
	else {
		return Rf_duplicate(x);
	}
}



// [[Rcpp::export]]
void C_set_altrep_class(SEXP class_symbol_name, SEXP type_name, bool redefineWarning) {
	altClassType class_type = get_altrep_enum_type_by_type_name(type_name);
	register_alt_class(class_symbol_name, class_type, redefineWarning);
}
// [[Rcpp::export]]
void C_set_alt_method(SEXP class_symbol_name, SEXP func_symbol_name, SEXP func,bool redefineWarning) {
	register_alt_method(class_symbol_name, func_symbol_name, func, redefineWarning);
}

// [[Rcpp::export]]
SEXP C_create_altrep(SEXP class_symbol_name, SEXP x) {
	//ERROR_WHEN_NOT_FIND_STR_KEY(altrep_name_map, class_name);
	if (!has_alt_class(class_symbol_name)) {
		errorHandle("The class '%s' is not found.\n", SYMBOL_TO_CHAR(class_symbol_name));
	}
	//prepare the data
	List state = List::create(Named("class_name") = class_symbol_name, Named("class_type") = (int)get_altrep_enum_type_by_class_symbol(class_symbol_name));
	R_altrep_class_t altrep_class = get_altrep_class(class_symbol_name);
	SEXP res = R_new_altrep(altrep_class, x, wrap(state));
	return res;
}

// [[Rcpp::export]]
SEXP C_get_alt_symbol_list() {
	return ALTREP_SYMBOL_LIST;
}

// [[Rcpp::export]]
SEXP C_get_valid_func_name() {
	get_valid_func_name();
}

// [[Rcpp::export]]
void C_initial_package(SEXP altrep_class_space,SEXP altrep_symbol_space) {
	ALTREP_CLASS_SPACE = altrep_class_space;
	ALTREP_SYMBOL_LIST = altrep_symbol_space;
	void init_altrep_symbol_list();
	init_altrep_symbol_list();
}

// [[Rcpp::export]]
void C_package_unload() {

}

// [[Rcpp::export]]
SEXP C_performace_test1(SEXP a,R_xlen_t n) {
	SEXP x;
	for (int i = 0; i < n; i++) {
		x=VECTOR_ELT(a, 10);
	}
	return x;
}

// [[Rcpp::export]]
SEXP C_performace_test2(SEXP env, SEXP sym, R_xlen_t n) {
	SEXP x;
	for (int i = 0; i < n; i++) {
		x = Rf_findVarInFrame(env, sym);
	}
	return x;
}

// [[Rcpp::export]]
SEXP C_test1(SEXP f, SEXP x) {
	SEXP call =PROTECT(Rf_lang2(f, x));
	SEXP val = R_forceAndCall(call, 1, R_GlobalEnv);
	UNPROTECT(1);
	return val;
}

// [[Rcpp::export]]
SEXP C_test2(SEXP expr, SEXP env) {
	SEXP val = Rf_eval(expr, env);
	return val;
}

// [[Rcpp::export]]
SEXP C_test3(SEXP f,SEXP x) {
	Function fun(f);
	return fun(x);
}

