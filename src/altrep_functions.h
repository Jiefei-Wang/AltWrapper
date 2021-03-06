#include "Rcpp.h"
#include "altrep.h"
#include "altrep_macro.h"
#include "tools.h"
#include <type_traits>
using namespace Rcpp;

//an all-in-one function to access the values of an altWrapper object
//The bool arguments indicate whether it is allowed to get the data from the corresponding function
//start must be specified if index is a null pointer. The function will first uses index, if not available it uses start.
template<class T>
int get_altrep_data(T* result, R_xlen_t n, SEXP x, R_xlen_t* index, R_xlen_t start, bool ptr, bool ptr_or_null, bool element, bool subset) {
	SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
	SEXP alt_class_space = GET_ALT_CLASS(alt_class_name_symbol);
	if (alt_class_space == R_UnboundValue)
		errorHandle("Unable to find the the class '%s'", SYMBOL_TO_CHAR(alt_class_name_symbol));
	if (ptr_or_null) {
		//Check whether the pointer is available
		SEXP func = GET_ALT_METHOD(alt_class_space, getDataptrOrNull);
		if (func != R_NilValue) {
			DEBUG(Rprintf("Alternatively access ptr_or_null\n"));
			SEXP res = make_call(func, GET_ALT_DATA(x), x);
			const T* ptr = nullptr;
			switch (TYPEOF(res)) {
			case NILSXP:
				break;
			case EXTPTRSXP:
				ptr = (const T*)R_ExternalPtrAddr(res);
			default:
				ptr = (const T*)DATAPTR_OR_NULL(res);
			}
			if (ptr != nullptr) {
				for (R_xlen_t i = 0; i < n; i++) {
					if (index != nullptr) {
						result[i] = ptr[index[i]];
					}
					else {
						result[i] = ptr[start + i];
					}
				}
				return 0;
			}
		}
	}

	//This is a shortcut for getting only one element
	if (element && n == 1) {
		//Check whether the element function is available
		SEXP func = GET_ALT_METHOD(alt_class_space, getElement);
		if (func != R_NilValue) {
			DEBUG(Rprintf("Alternatively access element\n"));
			SEXP res;
			if (index != nullptr) {
				res = make_call(func, GET_ALT_DATA(x), wrap(index[0] + 1), x);
			}
			else {
				res = make_call(func, GET_ALT_DATA(x), wrap(start + 1), x);
			}

			result[0] = as<T>(res);
			return 0;
		}
	}

	if (subset) {
		//Check whether the subset function is available
		SEXP func = GET_ALT_METHOD(alt_class_space, getSubset);
		if (func != R_NilValue) {
			DEBUG(Rprintf("Alternatively access subset\n"));
			NumericVector indx(10);
			for (R_xlen_t i = 0; i < n; i++) {
				if (index != nullptr) {
					indx[i] = index[i] + 1;
				}
				else {
					indx[i] = start + i + 1;
				}
			}
			NumericVector res = NumericVector(make_call(func, GET_ALT_DATA(x), wrap(indx), x));
			for (R_xlen_t i = 0; i < n; i++) {
				result[i] = res[i];
			}
			return 0;
		}
	}

	if (element) {
		//Check whether the element function is available
		SEXP func = GET_ALT_METHOD(alt_class_space, getElement);
		if (func != R_NilValue) {
			DEBUG(Rprintf("Alternatively access element\n"));
			for (R_xlen_t i = 0; i < n; i++) {
				SEXP res;
				if (index != nullptr) {
					res = make_call(func, GET_ALT_DATA(x), wrap(index[i] + 1), x);
				}
				else {
					res = make_call(func, GET_ALT_DATA(x), wrap(start + i + 1), x);
				}
				result[i] = as<T>(res);
			}
			return 0;
		}
	}

	if (ptr) {
		//Check whether the pointer is available
		SEXP func = GET_ALT_METHOD(alt_class_space, getDataptr);
		if (func != R_NilValue) {
			DEBUG(Rprintf("Alternatively access ptr\n"));
			SEXP R_writeable = PROTECT(wrap<int>(0));
			SEXP res = make_call(func, GET_ALT_DATA(x), R_writeable, x);
			UNPROTECT(1);
			T* ptr = nullptr;
			switch (TYPEOF(res)) {
			case NILSXP:
				break;
			case EXTPTRSXP:
				ptr = (T*)R_ExternalPtrAddr(res);
			default:
				ptr = (T*)DATAPTR(res);
			}
			if (ptr != nullptr) {
				for (R_xlen_t i = 0; i < n; i++) {
					if (index != nullptr) {
						result[i] = ptr[index[i]];
					}
					else {
						result[i] = ptr[start + i];
					}
				}
				return 0;
			}
		}
	}

	//If no function available, return -1
	return -1;
}




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
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, inspect);
		if (func != R_NilValue) {
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
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, getLength);
		if (func != R_NilValue) {
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
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, getDataptr);
		if (func != R_NilValue) {
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
		//Check if the pointer is accessable from dataptrOrNull function
		DEBUG(Rprintf("data pointer is not available. Auto accessing data pointer or null\n"));
		func = GET_ALT_METHOD(alt_class_space, getDataptrOrNull);
		Rf_PrintValue(func);
		if (func != R_NilValue) {
			SEXP res = make_call(func, GET_ALT_DATA(x), x);
			switch (TYPEOF(res)) {
			case NILSXP:
				return NULL;
			case EXTPTRSXP:
				return R_ExternalPtrAddr(res);
			default:
				return DATAPTR(res);
			}
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in dataptr: \n%s", ex.what());
	}
	errorHandle("cannot access data pointer for this ALTVEC object");
	return NULL;
}
const void* altrep_dataptr_or_null(SEXP x)
{
	DEBUG(Rprintf("accessing data pointer or null\n"));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, getDataptrOrNull);
		if (func != R_NilValue) {
			SEXP res = make_call(func, GET_ALT_DATA(x), x);
			switch (TYPEOF(res)) {
			case NILSXP:
				return NULL;
			case EXTPTRSXP:
				return R_ExternalPtrAddr(res);
			default:
				return DATAPTR_OR_NULL(res);
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
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, getSubset);
		if (func != R_NilValue) {
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
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, getElement);
		if (func != R_NilValue) {
			SEXP res = make_call(func, GET_ALT_DATA(x), wrap(i + 1), x);
			T returnValue = as<T>(res);
			//printf("value:%d\n", returnValue);
			//Rf_PrintValue(res);
			return returnValue;
		}
		else {
			T result;
			R_xlen_t index = i;
			if (get_altrep_data(&result, 1, x, &index, 0, true, true, false, true) == -1) {
				errorHandle("Get element function error: Unable to get the data from the ALTREP object.");
			}
			return result;
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in get element: \n%s", ex.what());
	}
	return 0;
}


SEXP wrap_out_pointer(void* out, R_xlen_t length, const char* type) {

	if (std::strcmp(type, "raw") == 0) {
		return R_new_altrep(altrep_internal_raw_class, R_MakeExternalPtr(out, R_NilValue, R_NilValue), wrap(length));
	}
	if (std::strcmp(type, "logical") == 0) {
		return R_new_altrep(altrep_internal_logical_class, R_MakeExternalPtr(out, R_NilValue, R_NilValue), wrap(length));
	}
	if (std::strcmp(type, "integer") == 0) {
		return R_new_altrep(altrep_internal_integer_class, R_MakeExternalPtr(out, R_NilValue, R_NilValue), wrap(length));
	}
	if (std::strcmp(type, "double") == 0 || std::strcmp(type, "real") == 0) {
		return R_new_altrep(altrep_internal_real_class, R_MakeExternalPtr(out, R_NilValue, R_NilValue), wrap(length));
	}
	errorHandle("Unsupported output type\n");
	return NULL;
}
	

template<class T>
R_xlen_t numeric_region(SEXP x, R_xlen_t start, R_xlen_t size, T * out) {
	DEBUG(Rprintf("accessing numeric region\n"));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, getRegion);
		if (func != R_NilValue) {
			const char* alt_class_type = CHARSXP_TO_CHAR(GET_ALT_CLASS_TYPE(x));
			SEXP output = PROTECT(wrap_out_pointer(out, size, alt_class_type));
			SEXP res = make_call(func, GET_ALT_DATA(x), wrap(start + 1), wrap(size), output, x);
			UNPROTECT(1);
			return as<R_xlen_t>(res);
		}
		else {
			R_xlen_t n = XLENGTH(x);
			R_xlen_t ncopy = n - start > size ? n : n - start;
			if (get_altrep_data(out, ncopy, x, nullptr, start, true, true, true, true) == -1) {
				errorHandle("Get region function error: Unable to get the data from the ALTREP object.");
			}
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
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, duplicate);
		if (func != R_NilValue) {
			SEXP R_deep = PROTECT(wrap<int>(deep));
			SEXP res = make_call(func, GET_ALT_DATA(x), R_deep, x);
			UNPROTECT(1);
			return res;
		}
		else {
			if (GET_ALT_CLASS_SETTING_DUPLICATE(alt_class_name_symbol)) {
				DEBUG(Rprintf("Auto duplicate\n"));
				SEXP class_type = GET_ALT_CLASS_TYPE(x);
				R_altrep_class_t altrep_class = get_altrep_class(class_type);
				SEXP data;
				if (deep) {
					data = PROTECT(Rf_duplicate(R_altrep_data1(x)));
				}
				else {
					data = PROTECT(Rf_shallow_duplicate(R_altrep_data1(x)));
				}
				
				SEXP duplicated_object = R_new_altrep(altrep_class, data, R_altrep_data2(x));
				UNPROTECT(1);
				return(duplicated_object);
			}
			else {
				return NULL;
			}
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
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, coerce);
		if (func != R_NilValue) {
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
	DEBUG(Rprintf("serializing data\n"));
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, serialize);
		SEXP state;
		if (func != R_NilValue) {
			state = PROTECT(make_call(func, GET_ALT_DATA(x), x));
		}
		else {
			//If a user has not provided a serialize function, use the default if auto serialize is on
			if (GET_ALT_CLASS_SETTING_SERIALIZE(alt_class_name_symbol)) {
				DEBUG(Rprintf("Auto serializing data\n"));
				state = PROTECT(wrap(List::create(R_altrep_data1(x), R_altrep_data2(x))));
			}
			else {
				//If the auto serialize function has been turned off, return NULL
				return NULL;
			}
		}
		//printf("check\n");
		//Create character class name since symbol name will be recognized as a variable
		SEXP alt_class_name = PROTECT(wrap(SYMBOL_TO_CHAR(alt_class_name_symbol)));
		//call package serialize function
		Environment package_env(PACKAGE_NAMESPACE);
		Function serializeAltWrapper = package_env[".serializeAltWrapper"];
		SEXP res = serializeAltWrapper(alt_class_name, state);
		UNPROTECT(2);
		return res;
	}
	catch (const std::exception & ex) {
		errorHandle("error in serialize: \n%s", ex.what());
	}
	return NULL;
}

static void loadLibrary() {
	SEXP e;
	Rf_protect(e = Rf_lang2(Rf_install("library"), Rf_mkString(PACKAGE_NAME)));
	R_tryEval(e, R_GlobalEnv, NULL);
	Rf_unprotect(1);
}

SEXP altrep_unserialize(SEXP R_class, SEXP serializedInfo) {
	DEBUG(Rprintf("unserializing data\n"););

	//Rf_PrintValue(serializedInfo);
	try {
		loadLibrary();
		Environment package_env(PACKAGE_NAMESPACE);
		Function unserializeAltWrapper = package_env[".unserializeAltWrapper"];
		unserializeAltWrapper(serializedInfo);
		//Rf_PrintValue(serializedInfo);

		List serializedInfoList = serializedInfo;
		SEXP state = serializedInfoList["state"];
		SEXP alt_class_name_symbol = serializedInfoList["className"];
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, unserialize);
		if (func != R_NilValue) {
			SEXP res = make_call(func, R_class, state, R_NilValue);
			return res;
		}
		else {
			//Check if auto serialize is on
			if (GET_ALT_CLASS_SETTING_SERIALIZE(alt_class_name_symbol)) {
				DEBUG(Rprintf("Auto unserializing data\n"));
				if (HAS_ALT_CLASS(alt_class_name_symbol)) {
					SEXP class_type = GET_ALT_CLASS_TYPE_BY_NAME(alt_class_name_symbol);
					R_altrep_class_t altrep_class = get_altrep_class(class_type);
					SEXP res = R_new_altrep(altrep_class, VECTOR_ELT(state, 0), VECTOR_ELT(state, 1));
					return(res);
				}
				else {
					errorHandle("The class %s has not been defined", SYMBOL_TO_CHAR(alt_class_name_symbol));
				}
			}
			else {
				errorHandle("cannot unserialize this ALTREP object yet");
			}
		}
	}
	catch (const std::exception & ex) {
		errorHandle("error in unserialize: \n%s", ex.what());
	}
	return NULL;
}



int altrep_is_sorted(SEXP x) {
	DEBUG(Rprintf("is_sorted function\n"););
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, isSorted);
		if (func != R_NilValue) {
			SEXP res = PROTECT(make_call(func, GET_ALT_DATA(x), x));
			UNPROTECT(1);
			return as<int>(res);
		}
		else {
			return UNKNOWN_SORTEDNESS;
		}
	}
	catch (const std::exception& ex) {
		errorHandle("error in serialize: \n%s", ex.what());
	}
	return UNKNOWN_SORTEDNESS;


}

int altrep_no_NA(SEXP x) {
	DEBUG(Rprintf("no_NA function\n"););
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, noNA);
		if (func != R_NilValue) {
			SEXP res = PROTECT(make_call(func, GET_ALT_DATA(x), x));
			UNPROTECT(1);
			return as<int>(res);
		}
		else {
			return 0;
		}
	}
	catch (const std::exception& ex) {
		errorHandle("error in serialize: \n%s", ex.what());
	}
	return 0;
}

SEXP altrep_sum(SEXP x, Rboolean na_rm) {
	DEBUG(Rprintf("sum function\n"););
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, sum);
		if (func != R_NilValue) {
			SEXP R_na_rm = PROTECT(wrap((bool)na_rm));
			SEXP res = PROTECT(make_call(func, GET_ALT_DATA(x), R_na_rm, x));
			UNPROTECT(2);
			return res;
		}
		else {
			return NULL;
		}
	}
	catch (const std::exception& ex) {
		errorHandle("error in serialize: \n%s", ex.what());
	}
	return NULL;
}

SEXP altrep_min(SEXP x, Rboolean na_rm) {
	DEBUG(Rprintf("min function\n"););
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, min);
		if (func != R_NilValue) {
			SEXP R_na_rm = PROTECT(wrap((bool)na_rm));
			SEXP res = PROTECT(make_call(func, GET_ALT_DATA(x), R_na_rm, x));
			UNPROTECT(2);
			return res;
		}
		else {
			return NULL;
		}
	}
	catch (const std::exception& ex) {
		errorHandle("error in serialize: \n%s", ex.what());
	}
	return NULL;
}

SEXP altrep_max(SEXP x, Rboolean na_rm) {
	DEBUG(Rprintf("max function\n"););
	try {
		SEXP alt_class_name_symbol = GET_ALT_CLASS_NAME_SYMBOL(x);
		ERROR_WHEN_NOT_FIND_ALT_CLASS(func, alt_class_name_symbol, max);
		if (func != R_NilValue) {
			SEXP R_na_rm = PROTECT(wrap((bool)na_rm));
			SEXP res = PROTECT(make_call(func, GET_ALT_DATA(x), R_na_rm, x));
			UNPROTECT(2);
			return res;
		}
		else {
			return NULL;
		}
	}
	catch (const std::exception& ex) {
		errorHandle("error in serialize: \n%s", ex.what());
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
