#include "Rcpp.h"
#include <map>
#include "altrep.h"
#include "tools.h"
using namespace Rcpp;
using std::map;
using std::pair;

#define RETURN_NULL_IF_NO_KEY(map,key)\
if (map.find(key) == map.end())return R_NilValue; 


Rboolean altrep_inspect(SEXP x, int pre, int deep, int pvec,
	void (*inspect_subtree)(SEXP, int, int, int)) {
	try {
		altClassKey key = as<altClassKey>(GET_ALT_CLASS_KEY(x));
		ERROR_WHEN_NOT_FIND_INT_KEY(altrep_length_map, key);
		SEXP call = PROTECT(Rf_lang2(altrep_inspect_map[key], GET_ALT_DATA(x)));
		R_forceAndCall(call, 1, R_GlobalEnv);
		UNPROTECT(1);
		return TRUE;
	}
	catch (const std::exception & ex) {
		errorHandle("error in inspect: \n%s", ex.what());
	}
	return FALSE;
}
R_xlen_t altrep_length(SEXP x) {
	DEBUG(Rprintf("accessing length\n"));
	try {
		altClassKey key = as<altClassKey>(GET_ALT_CLASS_KEY(x));
		ERROR_WHEN_NOT_FIND_INT_KEY(altrep_length_map, key);
		SEXP call = PROTECT(Rf_lang2(altrep_length_map[key], GET_ALT_DATA(x)));
		SEXP res = R_forceAndCall(call, 1, R_GlobalEnv);
		UNPROTECT(1);
		return as<R_xlen_t>(res);
	}
	catch (const std::exception & ex) {
		errorHandle("error in length: \n%s", ex.what());
	}
	return 0;
}


void* altrep_dataptr(SEXP x, Rboolean writeable) {
	DEBUG(Rprintf("accessing data pointer\n"));
	try {
		altClassKey key = as<altClassKey>(GET_ALT_CLASS_KEY(x));
		ERROR_WHEN_NOT_FIND_INT_KEY(altrep_dataptr_map, key);
		SEXP R_writeable = wrap<int>(writeable);
		SEXP call = PROTECT(Rf_lang3(altrep_dataptr_map[key], GET_ALT_DATA(x), R_writeable));
		SEXP res = R_forceAndCall(call, 2, R_GlobalEnv);
		UNPROTECT(1);
		switch (TYPEOF(res)) {
		case EXTPTRSXP: 
			return R_ExternalPtrAddr(res);
		default:
			return dataptr(res);
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in dataptr: \n%s", ex.what());
	}
	return NULL;
}
const void* altrep_dataptr_or_null(SEXP x)
{
	DEBUG(Rprintf("accessing data pointer or null\n"));
	try {
		altClassKey key = as<altClassKey>(GET_ALT_CLASS_KEY(x));
		RETURN_NULL_IF_NO_KEY(altrep_dataptr_or_null_map, key);
		SEXP call = PROTECT(Rf_lang2(altrep_dataptr_or_null_map[key], GET_ALT_DATA(x)));
		SEXP res = R_forceAndCall(call, 1, R_GlobalEnv);
		UNPROTECT(1);
		switch (TYPEOF(res)) {
		case NILSXP:
			return NULL;
		case EXTPTRSXP:
			return R_ExternalPtrAddr(res);
		default:
			return dataptr(res);
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in dataptr or null:\n%s", ex.what());
	}
	return NULL;
}

SEXP altrep_coerce(SEXP x, int type) {
	DEBUG(Rprintf("Coercing data\n"));
	try {
		altClassKey key = as<altClassKey>(GET_ALT_CLASS_KEY(x));
		RETURN_NULL_IF_NO_KEY(altrep_coerce_map, key);
		SEXP R_type = wrap<int>(type);
		SEXP call = PROTECT(Rf_lang3(altrep_coerce_map[key], GET_ALT_DATA(x), R_type));
		SEXP res = R_forceAndCall(call, 2, R_GlobalEnv);
		UNPROTECT(1);
		return res;
	}
	catch (const std::exception & ex) {
		errorHandle("error in coerce: \n%s", ex.what());
	}
	return R_NilValue;
}

SEXP altrep_duplicate(SEXP x, Rboolean deep) {
	DEBUG(Rprintf("Duplicating data, deep: %d\n", deep));
	try {
		altClassKey key = as<altClassKey>(GET_ALT_CLASS_KEY(x));
		RETURN_NULL_IF_NO_KEY(altrep_duplicate_map, key);
		SEXP R_deep = wrap<int>(deep);
		SEXP call = PROTECT(Rf_lang3(altrep_duplicate_map[key], GET_ALT_DATA(x), R_deep));
		SEXP res = R_forceAndCall(call, 2, R_GlobalEnv);
		UNPROTECT(1);
		return res;
	}
	catch (const std::exception & ex) {
		errorHandle("error in duplicate: \n%s", ex.what());
	}
	return R_NilValue;
}

SEXP altrep_serialize_state(SEXP x) {
	DEBUG(Rprintf("serializing data\n"););
		try {
		altClassKey key = as<altClassKey>(GET_ALT_CLASS_KEY(x));
		RETURN_NULL_IF_NO_KEY(altrep_serialize_map, key);
		SEXP call = PROTECT(Rf_lang2(altrep_serialize_map[key], GET_ALT_DATA(x)));
		SEXP res = R_forceAndCall(call, 1, R_GlobalEnv);
		SEXP state = PROTECT(Rf_allocVector(VECSXP, 2));
		SET_VECTOR_ELT(state, 0, wrap(key));
		SET_VECTOR_ELT(state, 1, res);
		UNPROTECT(2);
		return state;
	}
	catch (const std::exception & ex) {
		errorHandle("error in serialize: \n%s", ex.what());
	}
	return R_NilValue;
}


SEXP altrep_unserialize(SEXP R_class, SEXP state) {
	DEBUG(Rprintf("unserializing data\n"););
		try {
		altClassKey key = as<altClassKey>(VECTOR_ELT(state,0));
		ERROR_WHEN_NOT_FIND_INT_KEY(altrep_unserialize_map, key);
		SEXP call = PROTECT(Rf_lang3(altrep_unserialize_map[key], R_class,VECTOR_ELT(state, 1)));
		SEXP res = R_forceAndCall(call, 2, R_GlobalEnv);
		UNPROTECT(1);
		return res;
	}
	catch (const std::exception & ex) {
		errorHandle("error in unserialize: \n%s", ex.what());
	}
	return R_NilValue;
}


SEXP altrep_subset(SEXP x, SEXP indx, SEXP call_stack) {
	DEBUG(Rprintf("subsetting data\n"););
	try {
		altClassKey key = as<altClassKey>(GET_ALT_CLASS_KEY(x));
		ERROR_WHEN_NOT_FIND_INT_KEY(altrep_subset_map, key);
		//NumericVector index(indx);
		//index = index + 1;
		SEXP call = PROTECT(Rf_lang3(altrep_subset_map[key], GET_ALT_DATA(x), indx));
		SEXP res = R_forceAndCall(call, 2, R_GlobalEnv);
		UNPROTECT(1);
		return res;
	}
	catch (const std::exception& ex) {
		errorHandle("error in subset: \n%s", ex.what());
	}

	return R_NilValue;
}

R_xlen_t altrep_internal_length(SEXP x) {
	return as<R_xlen_t>(R_altrep_data2(x));
}

void* altrep_internal_dataptr(SEXP x, Rboolean writeable) {
	void* ptr = R_ExternalPtrAddr(R_altrep_data1(x));
	return ptr;
}

SEXP altrep_internal_duplicate(SEXP x, Rboolean deep) {
	return x;
}
