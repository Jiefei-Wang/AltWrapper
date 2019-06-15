#include <Rcpp.h>
#include "altrep.h"
#include "tools.h"

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
SEXP C_create_altrep(string class_name, SEXP x) {
	ERROR_WHEN_NOT_FIND_STR_KEY(altrep_name_map, class_name);
	//prepare the data
	altClassKey key = altrep_name_map[class_name];
	altClassType class_type = altrep_type_map[key];
	List state = List::create(Named("class_str") = class_name, Named("class_key") = key);
	SEXP res;
	switch (class_type) {
	case real:
		res = R_new_altrep(altrep_real_class, x, wrap(state));
		break;
	default:
		errorHandle("Unsupported altrep type\n");
	}

	return res;
}
// [[Rcpp::export]]
void C_set_altrep_class(string class_name, string type) {
	altClassType class_type = get_altrep_type(type);
	register_class(class_name, class_type);
}

#define SET_ALTREP_METHOD(method)\
void C_set_altrep_##method##_method(string class_name, SEXP func) {\
ERROR_WHEN_NOT_FIND_STR_KEY(altrep_name_map, class_name);\
altClassKey key = altrep_name_map[class_name];\
if(HAS_KEY(altrep_##method##_map,key)) {\
Rprintf("The function `%s` for the class `%s` has been defined and will be replaced by the new function",#method,class_name.c_str());\
altrep_##method##_map.erase(key);\
}\
altrep_##method##_map.insert(std::pair<altClassKey, SEXP>(key, func));\
}


SET_ALTREP_METHOD(inspect)
SET_ALTREP_METHOD(length)
SET_ALTREP_METHOD(duplicate)
SET_ALTREP_METHOD(coerce)
SET_ALTREP_METHOD(serialize)
SET_ALTREP_METHOD(unserialize)
SET_ALTREP_METHOD(dataptr)
SET_ALTREP_METHOD(dataptr_or_null)
SET_ALTREP_METHOD(subset)
SET_ALTREP_METHOD(get_element)
SET_ALTREP_METHOD(region)



// [[Rcpp::export]]
void C_set_altrep_inspect_method(string class_name, SEXP func);
// [[Rcpp::export]]
void C_set_altrep_length_method(string class_name, SEXP func);
// [[Rcpp::export]]
void C_set_altrep_duplicate_method(string class_name, SEXP func);
// [[Rcpp::export]]
void C_set_altrep_coerce_method(string class_name, SEXP func);
// [[Rcpp::export]]
void C_set_altrep_serialize_method(string class_name, SEXP func);
// [[Rcpp::export]]
void C_set_altrep_unserialize_method(string class_name, SEXP func);
// [[Rcpp::export]]
void C_set_altrep_dataptr_method(string class_name, SEXP func);
// [[Rcpp::export]]
void C_set_altrep_dataptr_or_null_method(string class_name, SEXP func);
// [[Rcpp::export]]
void C_set_altrep_subset_method(string class_name, SEXP func);
// [[Rcpp::export]]
void C_set_altrep_get_element_method(string class_name, SEXP func);
// [[Rcpp::export]]
void C_set_altrep_region_method(string class_name, SEXP func);


// [[Rcpp::export]]
void C_check_altrep_method() {
	for (std::map<std::string, altClassKey>::iterator it = altrep_name_map.begin(); it != altrep_name_map.end(); ++it) {
		print_class(it->first);
	}
}


// [[Rcpp::export]]
SEXP C_test1(SEXP f, SEXP x, SEXP env) {
	SEXP call =PROTECT(Rf_lang2(f, x));
	SEXP val = R_forceAndCall(call, 1, env);
	UNPROTECT(1);
	return val;
}

// [[Rcpp::export]]
SEXP C_test2(SEXP expr, SEXP env) {
	SEXP val = Rf_eval(expr, env);
	return val;
}
Function* fun;
// [[Rcpp::export]]
void C_setFunc(SEXP f) {
	fun = new Function(f);
}

// [[Rcpp::export]]
SEXP C_test3(SEXP x) {
	return (*fun)(x);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/

// [[Rcpp::export]]
void I_know_it_is_not_correct(SEXP x,SEXP attrName) {
	INTEGER(Rf_getAttrib(x, attrName))[1]=2;
}
