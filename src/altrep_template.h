#include <type_traits>
using namespace Rcpp;
using std::map;
using std::pair;

template<class T>
T altrep_get_element(SEXP x, R_xlen_t i) {
	DEBUG(Rprintf("accessing element\n"));
	try {
		altClassKey key = as<altClassKey>(GET_ALT_CLASS_KEY(x));
		ERROR_WHEN_NOT_FIND_INT_KEY(altrep_get_element_map, key);
		SEXP call = PROTECT(Rf_lang3(altrep_get_element_map[key], GET_ALT_DATA(x), wrap(i + 1)));
		SEXP res = R_forceAndCall(call, 2, R_GlobalEnv);
		UNPROTECT(1);
		return as<T>(res);
	}
	catch (const std::exception & ex) {
		errorHandle("error in get element: \n%s", ex.what());
	}
	return 0;
}

template<class T>
SEXP wrap_out_pointer(T* out, R_xlen_t size) {
	if (std::is_same<T, double>::value) {
		return PROTECT(R_new_altrep(altrep_internal_real_class, R_MakeExternalPtr(out,R_NilValue, R_NilValue), wrap(size)));
	}
	if (std::is_same<T, int>::value) {
		return PROTECT(R_new_altrep(altrep_internal_integer_class, R_MakeExternalPtr(out, R_NilValue, R_NilValue), wrap(size)));
	}
	errorHandle("Unsupported output type\n");
}

template<class T>
R_xlen_t numeric_region(SEXP x, R_xlen_t start, R_xlen_t size, T* out) {
	DEBUG(Rprintf("accessing numeric region\n"));
	try {
	altClassKey key = as<altClassKey>(GET_ALT_CLASS_KEY(x));
	if (HAS_KEY(altrep_region_map, key)) {
		SEXP output = wrap_out_pointer(out,size);
		SEXP call = PROTECT(Rf_lang5(altrep_region_map[key], GET_ALT_DATA(x), wrap(start + 1), wrap(size), output));
		SEXP res = R_forceAndCall(call, 4, R_GlobalEnv);
		UNPROTECT(2);
		return as<R_xlen_t>(res);
	}
	else {
		const T* x_ptr = (const T*)DATAPTR_OR_NULL(x);
		if(DATAPTR_OR_NULL(x) ==NULL) 
			errorHandle("The data pointer or null function returns NULL. For the performance reason, unable to use the default method for the region function\n");
		R_xlen_t n = XLENGTH(x);
		R_xlen_t ncopy = n - start > size ? n : n - start;
		for (R_xlen_t k = 0; k < ncopy; k++)
			out[k] = x_ptr[k + start];
		return ncopy;
	}
	}
	catch (const std::exception& ex) {
		errorHandle("error in get element: \n%s", ex.what());
	}
	return 0;
	
}