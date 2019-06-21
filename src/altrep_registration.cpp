#include "Rcpp.h"
#include "R_ext/Altrep.h"
#include "altrep.h"
#include "tools.h"
#include "altrep_macro.h"
#include "altrep_common_func.h"
#include "altrep_template.h"

using namespace Rcpp;

SEXP ALTREP_CLASS_SPACE;

R_altrep_class_t altrep_real_class;
R_altrep_class_t altrep_integer_class;


R_altrep_class_t altrep_internal_real_class;
R_altrep_class_t altrep_internal_integer_class;



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



//[[Rcpp::init]]
void init_altrep_dispatcher(DllInfo* dll) {
	R_altrep_class_t alt_class;
	const char* class_name;

	class_name = "alt_real";
	alt_class = R_make_altreal_class(class_name, PACKAGE_NAME, dll);
	ALTREP_COMMOM_REGISTRATION(alt_class);
	R_set_altreal_Elt_method(alt_class, altrep_get_element<double>);
	R_set_altreal_Get_region_method(alt_class, numeric_region<double>);
	altrep_real_class = alt_class;


	class_name = "alt_integer";
	alt_class = R_make_altreal_class(class_name, PACKAGE_NAME, dll);
	ALTREP_COMMOM_REGISTRATION(alt_class);
	R_set_altinteger_Elt_method(alt_class, altrep_get_element<int>);
	R_set_altinteger_Get_region_method(alt_class, numeric_region<int>);
	altrep_integer_class = alt_class;


}

//The internal altrep is a wrapper for the pointer type variable in C++
//[[Rcpp::init]]
void init_altrep_internal_class(DllInfo* dll) {
	const char* class_name;
	R_altrep_class_t alt_class;

	class_name = "alt_internal_real";
	alt_class = R_make_altreal_class(class_name, PACKAGE_NAME, dll);
	R_set_altrep_Length_method(alt_class, altrep_internal_length);
	R_set_altrep_Duplicate_method(alt_class, altrep_internal_duplicate);
	R_set_altvec_Dataptr_method(alt_class, altrep_internal_dataptr);
	altrep_internal_real_class = alt_class;

	class_name = "alt_internal_integer";
	alt_class = R_make_altinteger_class(class_name, PACKAGE_NAME, dll);
	R_set_altrep_Length_method(alt_class, altrep_internal_length);
	R_set_altrep_Duplicate_method(alt_class, altrep_internal_duplicate);
	R_set_altvec_Dataptr_method(alt_class, altrep_internal_dataptr);
	altrep_internal_integer_class = alt_class;
}





