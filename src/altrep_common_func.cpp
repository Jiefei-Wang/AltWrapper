#include "Rcpp.h"
#include <map>
#include "altrep.h"
#include "altrep_macro.h"
#include "altrep_tools.h"
#include "tools.h"
using namespace Rcpp;
using std::map;
using std::pair;



Rboolean altrep_inspect(SEXP x, int pre, int deep, int pvec,
	void (*inspect_subtree)(SEXP, int, int, int)) {
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol =  GET_ALT_SYMBOL(inspect);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func!=R_UnboundValue) {
			make_call(func, GET_ALT_DATA(x));
			return TRUE;
		}
		else {
			return FALSE;
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in inspect: \n%s", ex.what());
	}
	return FALSE;
}



R_xlen_t altrep_length(SEXP x) {
	DEBUG(Rprintf("accessing length\n"));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(length);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res =make_call(func, GET_ALT_DATA(x));
			return as<R_xlen_t>(res);
		}
		else {
			errorHandle("no Length method defined");
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in length: \n%s", ex.what());
	}
	return 0;
}


void* altrep_dataptr(SEXP x, Rboolean writeable) {
	DEBUG(Rprintf("accessing data pointer\n"));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(dataptr);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP R_writeable = PROTECT(wrap<int>(writeable));
			SEXP res = make_call(func, GET_ALT_DATA(x), R_writeable);
			UNPROTECT(1);
			switch(TYPEOF(res)) {
			case EXTPTRSXP:
				return R_ExternalPtrAddr(res);
			default:
				return dataptr(res);
			}
		}
		else {
			errorHandle("cannot access data pointer for this ALTVEC object");
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
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(dataptr_or_null);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res = make_call(func, GET_ALT_DATA(x));
			switch (TYPEOF(res)) {
			case NILSXP:
				return NULL;
			case EXTPTRSXP:
				return R_ExternalPtrAddr(res);
			default:
				return dataptr(res);
			}
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
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(coerce);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP R_type = PROTECT(wrap<int>(type));
			SEXP res=make_call(func, GET_ALT_DATA(x), R_type);
			UNPROTECT(1);
			switch (TYPEOF(res)) {
			case NILSXP:
				return NULL;
			default:
				return res;
			}
		}
		else {
			return NULL;
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in coerce: \n%s", ex.what());
	}
	return NULL;
}

SEXP altrep_duplicate(SEXP x, Rboolean deep) {
	DEBUG(Rprintf("Duplicating data, deep: %d\n", deep));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(duplicate);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP R_deep = PROTECT(wrap<int>(deep));
			SEXP res = make_call(func, GET_ALT_DATA(x), R_deep);
			UNPROTECT(1);
			return res;
		}
		else {
			return NULL;
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in duplicate: \n%s", ex.what());
	}
	return NULL;
}



SEXP altrep_serialize_state(SEXP x) {
	DEBUG(Rprintf("serializing data\n"););
		try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(serialize);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res = PROTECT(make_call(func, GET_ALT_DATA(x)));
			SEXP state = PROTECT(Rf_allocVector(VECSXP, 3));
			//Not implemented
			SET_VECTOR_ELT(state, 0, wrap(alt_class_name_symbol));
			SET_VECTOR_ELT(state, 1, wrap(alt_class_name_symbol));
			SET_VECTOR_ELT(state, 2, res);
			UNPROTECT(2);
			return res;
		}
		else {
			return NULL;
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in serialize: \n%s", ex.what());
	} 
	return NULL;
}


SEXP altrep_unserialize(SEXP R_class, SEXP state) {
	DEBUG(Rprintf("unserializing data\n"););
		try {
		SEXP alt_class_name_symbol = VECTOR_ELT(state, 0);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(unserialize);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res = make_call(func, R_class, VECTOR_ELT(state, 2));
			return res;
		}
		else {
			errorHandle("cannot unserialize this ALTREP object yet");
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in unserialize: \n%s", ex.what());
	}
	return NULL;
}


SEXP altrep_subset(SEXP x, SEXP indx, SEXP call_stack) {
	DEBUG(Rprintf("subsetting data\n"););
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(subset);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res = make_call(func, GET_ALT_DATA(x), indx);
			return res;
		}
		else {
			return NULL;
		}
	}
	catch (const std::exception& ex) {
		errorHandle("error in subset: \n%s", ex.what());
	}
	return NULL;
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
