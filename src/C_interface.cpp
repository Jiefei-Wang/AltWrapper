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


void C_attachAttr(SEXP x, SEXP R_tag, SEXP R_attr) {
	const char* tag = R_CHAR(Rf_asChar(R_tag));
	Rf_setAttrib(x, Rf_install(tag), R_attr);
}


// [[Rcpp::export]]
SEXP C_create_altrep(SEXP class_symbol_name, SEXP x,SEXP class_type,SEXP state, SEXP attrName, SEXP attributes) {
	R_altrep_class_t altrep_class = get_altrep_class(class_type);
	SEXP res = PROTECT(R_new_altrep(altrep_class, x, state));
	for (int i = 0; i < LENGTH(attrName); i++) {
		C_attachAttr(res, STRING_ELT(attrName, i), VECTOR_ELT(attributes, i));
	}
	UNPROTECT(1);
	//SET_NAMED(res, 0);
	return res;
}




static void ptr_finalizer(SEXP extPtr) {
	DEBUG(Rprintf("Finalizing data pointer\n"));
	void* ptr = R_ExternalPtrAddr(extPtr);
	free(ptr);
}
// [[Rcpp::export]]
SEXP C_create_internal_altrep(SEXP class_type, R_xlen_t length) {
	R_altrep_class_t altrep_internal_class = get_altrep_internal_class(class_type);
	void* ptr = malloc(get_class_type_size(class_type) * length);
	SEXP x = PROTECT(R_MakeExternalPtr(ptr,R_NilValue,R_NilValue));
	R_RegisterCFinalizer(x, ptr_finalizer);

	SEXP res=R_new_altrep(altrep_internal_class, x, wrap(length));

	UNPROTECT(1);
	return res;
}

// [[Rcpp::export]]
int C_getName(SEXP x) {
	return NAMED(x);
}
// [[Rcpp::export]]
SEXP C_duplicate(SEXP x, bool shallow) {
	if (shallow) {
		return Rf_shallow_duplicate(x);
	}
	else {
		return Rf_duplicate(x);
	}
}

// [[Rcpp::export]]
void C_initial_package(SEXP altrep_class_space,SEXP altrep_symbol_space) {
	ALTREP_REGISTRY_ENVIRONMENT = altrep_class_space;
	ALTREP_SYMBOL_LIST = altrep_symbol_space;
}

// [[Rcpp::export]]
List C_get_sortness_macro() {
	List sortness_macro= List::create(Named("decreasing_NA_1st") = (int)SORTED_DECR_NA_1ST,
		Named("decreasing") = (int)SORTED_DECR,
		Named("unknown") = (int)UNKNOWN_SORTEDNESS,
		Named("increasing") = (int)SORTED_INCR,
		Named("increasing_NA_1st") = (int)SORTED_INCR_NA_1ST,
		Named("known_unsorted") = (int)KNOWN_UNSORTED);
	return(sortness_macro);
}

// [[Rcpp::export]]
List C_get_NA_status_macro() {
	List NA_status_macro = List::create(Named("unknown") = 0L,
		Named("no_NA") = 1L);
	return(NA_status_macro);
}


