#include "Rcpp.h"
#include "R_ext/Altrep.h"
#include "altrep.h"
#include "tools.h"
#include "altrep_macro.h"
#include "altrep_functions.h"

using namespace Rcpp;

SEXP ALTREP_REGISTRY_ENVIRONMENT;



R_altrep_class_t altrep_raw_class;
R_altrep_class_t altrep_logical_class;
R_altrep_class_t altrep_integer_class;
R_altrep_class_t altrep_real_class;


R_altrep_class_t altrep_internal_raw_class;
R_altrep_class_t altrep_internal_logical_class;
R_altrep_class_t altrep_internal_integer_class;
R_altrep_class_t altrep_internal_real_class;

#define ALTREP_COMMOM_REGISTRATION(ALT_CLASS)\
	/* override ALTREP methods */\
	R_set_altrep_Inspect_method(ALT_CLASS, altrep_inspect);\
	R_set_altrep_Length_method(ALT_CLASS, altrep_length);\
	R_set_altrep_Duplicate_method(ALT_CLASS, altrep_duplicate);\
	R_set_altrep_Coerce_method(ALT_CLASS, altrep_coerce);\
	R_set_altrep_Unserialize_method(ALT_CLASS, altrep_unserialize);\
	R_set_altrep_Serialized_state_method(ALT_CLASS, altrep_serialize_state);\
	/* override ALTVEC methods */\
	R_set_altvec_Dataptr_method(ALT_CLASS, altrep_dataptr);\
	R_set_altvec_Dataptr_or_null_method(ALT_CLASS, altrep_dataptr_or_null);\
	R_set_altvec_Extract_subset_method(ALT_CLASS, altrep_subset);

#define ALTREP_NUMERIC_REGISTRATION(ALT_CLASS,ALT_TYPE)\
	R_set_##ALT_TYPE##_Is_sorted_method(ALT_CLASS,altrep_is_sorted);\
	R_set_##ALT_TYPE##_No_NA_method(ALT_CLASS,altrep_no_NA);\
	R_set_##ALT_TYPE##_Sum_method(ALT_CLASS,altrep_sum);

#define ALTREP_MATH_REGISTRATION(ALT_CLASS,ALT_TYPE)\
	R_set_##ALT_TYPE##_Min_method(ALT_CLASS,altrep_min);\
	R_set_##ALT_TYPE##_Max_method(ALT_CLASS,altrep_max);


//[[Rcpp::init]]
void init_altrep_dispatcher(DllInfo* dll) {
	R_altrep_class_t alt_class;
	const char* class_name;


	class_name = "alt_raw";
	alt_class = R_make_altraw_class(class_name, PACKAGE_NAME, dll);
	ALTREP_COMMOM_REGISTRATION(alt_class);
	R_set_altraw_Elt_method(alt_class, altrep_get_element<Rbyte>);
	R_set_altraw_Get_region_method(alt_class, numeric_region<Rbyte>);
	altrep_raw_class = alt_class;


	class_name = "alt_logical";
	alt_class = R_make_altlogical_class(class_name, PACKAGE_NAME, dll);
	ALTREP_COMMOM_REGISTRATION(alt_class);
	ALTREP_NUMERIC_REGISTRATION(alt_class, altlogical);
	R_set_altlogical_Elt_method(alt_class, altrep_get_element<int>);
	R_set_altlogical_Get_region_method(alt_class, numeric_region<int>);
	altrep_logical_class = alt_class;

	class_name = "alt_integer";
	alt_class = R_make_altinteger_class(class_name, PACKAGE_NAME, dll);
	ALTREP_COMMOM_REGISTRATION(alt_class);
	ALTREP_NUMERIC_REGISTRATION(alt_class, altinteger);
	ALTREP_MATH_REGISTRATION(alt_class, altinteger);
	R_set_altinteger_Elt_method(alt_class, altrep_get_element<int>);
	R_set_altinteger_Get_region_method(alt_class, numeric_region<int>);
	altrep_integer_class = alt_class;

	class_name = "alt_real";
	alt_class = R_make_altreal_class(class_name, PACKAGE_NAME, dll);
	ALTREP_COMMOM_REGISTRATION(alt_class);
	ALTREP_NUMERIC_REGISTRATION(alt_class, altreal);
	ALTREP_MATH_REGISTRATION(alt_class, altreal);
	R_set_altreal_Elt_method(alt_class, altrep_get_element<double>);
	R_set_altreal_Get_region_method(alt_class, numeric_region<double>);
	altrep_real_class = alt_class;
}

//The internal altrep is a wrapper for the pointer type variable in C++
//[[Rcpp::init]]
void init_altrep_internal_class(DllInfo* dll) {
	const char* class_name;
	R_altrep_class_t alt_class;

	class_name = "alt_internal_raw";
	alt_class = R_make_altraw_class(class_name, PACKAGE_NAME, dll);
	R_set_altrep_Length_method(alt_class, altrep_internal_length);
	R_set_altrep_Duplicate_method(alt_class, altrep_internal_duplicate);
	R_set_altvec_Dataptr_method(alt_class, altrep_internal_dataptr);
	altrep_internal_raw_class = alt_class;

	class_name = "alt_internal_logical";
	alt_class = R_make_altlogical_class(class_name, PACKAGE_NAME, dll);
	R_set_altrep_Length_method(alt_class, altrep_internal_length);
	R_set_altrep_Duplicate_method(alt_class, altrep_internal_duplicate);
	R_set_altvec_Dataptr_method(alt_class, altrep_internal_dataptr);
	altrep_internal_logical_class = alt_class;

	class_name = "alt_internal_integer";
	alt_class = R_make_altinteger_class(class_name, PACKAGE_NAME, dll);
	R_set_altrep_Length_method(alt_class, altrep_internal_length);
	R_set_altrep_Duplicate_method(alt_class, altrep_internal_duplicate);
	R_set_altvec_Dataptr_method(alt_class, altrep_internal_dataptr);
	altrep_internal_integer_class = alt_class;

	class_name = "alt_internal_real";
	alt_class = R_make_altreal_class(class_name, PACKAGE_NAME, dll);
	R_set_altrep_Length_method(alt_class, altrep_internal_length);
	R_set_altrep_Duplicate_method(alt_class, altrep_internal_duplicate);
	R_set_altvec_Dataptr_method(alt_class, altrep_internal_dataptr);
	altrep_internal_real_class = alt_class;
}





