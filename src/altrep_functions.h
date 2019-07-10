#include "Rcpp.h"
#include <map>
#include "altrep.h"
#include "altrep_macro.h"
#include "altrep_tools.h"
#include "tools.h"
#include <type_traits>


using namespace Rcpp;
using std::map;
using std::pair;
/*
SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
SEXP alt_class_func_symbol = GET_ALT_SYMBOL(inspect);
ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
if (func != R_UnboundValue) {
	SEXP res = make_call(func, GET_ALT_DATA(x));
	return res;
}
else {
}
*/

/*
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
*/






Rboolean altrep_inspect(SEXP x, int pre, int deep, int pvec,
	void (*inspect_subtree)(SEXP, int, int, int)) {
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(inspect);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res=make_call(func, GET_ALT_DATA(x),x);
			if (TYPEOF(res) == LGLSXP) {
				return (Rboolean)as<bool>(res);
			}
			else {
				return TRUE;
			}
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
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(getLength);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res = make_call(func, GET_ALT_DATA(x), x);
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
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(getDataptr);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP R_writeable = PROTECT(wrap<int>(writeable));
			SEXP res = make_call(func, GET_ALT_DATA(x), R_writeable, x);
			UNPROTECT(1);
			switch (TYPEOF(res)) {
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
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(getDataptrOrNull);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res = make_call(func, GET_ALT_DATA(x), x);
			switch (TYPEOF(res)) {
			case NILSXP:
				return NULL;
			case EXTPTRSXP:
				return R_ExternalPtrAddr(res);
			default:
				//double* ptr = (double*)DATAPTR(res);
				//printf("check,%f \n", ptr[0]);
				return DATAPTR(res);
			}
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in dataptr or null:\n%s", ex.what());
	}
	return NULL;
}


SEXP altrep_subset(SEXP x, SEXP indx, SEXP call_stack) {
	DEBUG(Rprintf("subsetting data\n"););
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(getSubset);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res = make_call(func, GET_ALT_DATA(x), indx, x);
			return res;
		}
		else {
			return NULL;
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in subset: \n%s", ex.what());
	}
	return NULL;
}

template<class T>
T altrep_get_element(SEXP x, R_xlen_t i) {
	DEBUG(Rprintf("accessing element %d\n", (int)i));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(getElement);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res = make_call(func, GET_ALT_DATA(x), wrap(i + 1), x);
			T returnValue = as<T>(res);
			//printf("value:%d\n", returnValue);
			//Rf_PrintValue(res);
			return returnValue;
		}
		else {
			T* ptr = (T*)DATAPTR_OR_NULL(x);
			if (ptr == NULL) {
				errorHandle("Unable to get the data pointer from dataptr_or_null.");
			}
			return ptr[i];
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in get element: \n%s", ex.what());
	}
	return 0;
}

template<class T>
SEXP wrap_out_pointer(T * out, R_xlen_t size) {
	Environment package_env(PACKAGE_ENV_NAME);
	if (std::is_same<T, double>::value) {
		return R_new_altrep(altrep_internal_real_class, R_MakeExternalPtr(out, R_NilValue, R_NilValue), wrap(size));
	}
	if (std::is_same<T, int>::value) {
		return R_new_altrep(altrep_internal_integer_class, R_MakeExternalPtr(out, R_NilValue, R_NilValue), wrap(size));
	}
	errorHandle("Unsupported output type\n");
}

template<class T>
R_xlen_t numeric_region(SEXP x, R_xlen_t start, R_xlen_t size, T * out) {
	DEBUG(Rprintf("accessing numeric region\n"));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(getRegion);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP output = PROTECT(wrap_out_pointer(out, size));
			SEXP res = make_call(func, GET_ALT_DATA(x), wrap(start + 1), wrap(size), output, x);
			UNPROTECT(1);
			return as<R_xlen_t>(res);
		}
		else {
			const T* x_ptr = (const T*)DATAPTR_OR_NULL(x);
			if (DATAPTR_OR_NULL(x) == NULL)
				errorHandle("The data pointer or null function returns NULL. For the performance reason, unable to use the default method for the region function\n");
			R_xlen_t n = XLENGTH(x);
			R_xlen_t ncopy = n - start > size ? n : n - start;
			for (R_xlen_t k = 0; k < ncopy; k++)
				out[k] = x_ptr[k + start];
			return ncopy;
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in get element: \n%s", ex.what());
	}
	return 0;
}



SEXP altrep_duplicate(SEXP x, Rboolean deep) {
	DEBUG(Rprintf("Duplicating data, deep: %d\n", deep));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(duplicate);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP R_deep = PROTECT(wrap<int>(deep));
			SEXP res = make_call(func, GET_ALT_DATA(x), R_deep, x);
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


SEXP altrep_coerce(SEXP x, int type) {
	DEBUG(Rprintf("Coercing data\n"));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(coerce);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP R_type = PROTECT(wrap<int>(type));
			SEXP res = make_call(func, GET_ALT_DATA(x), R_type, x);
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


SEXP altrep_serialize_state(SEXP x) {
	DEBUG(Rprintf("serializing data\n"););
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(serialize);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res = PROTECT(make_call(func, GET_ALT_DATA(x), x));
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
			SEXP res = make_call(func, R_class, VECTOR_ELT(state, 2), R_NilValue);
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

