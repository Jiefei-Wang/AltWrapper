#include <type_traits>
#include "altrep_tools.h"
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



template<class T>
T altrep_get_element(SEXP x, R_xlen_t i) {
	DEBUG(Rprintf("accessing element\n"));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(get_element);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP res = make_call(func, GET_ALT_DATA(x), wrap(i + 1));
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
		SEXP alt_class_func_symbol = GET_ALT_SYMBOL(region);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, alt_class_func_symbol);
		if (func != R_UnboundValue) {
			SEXP output = PROTECT(wrap_out_pointer(out, size));
			SEXP res = make_call(func, GET_ALT_DATA(x), wrap(start + 1), wrap(size), output);
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