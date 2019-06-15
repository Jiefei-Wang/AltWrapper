#include "Rcpp.h"
#include "R_ext/Altrep.h"
#include <map>
#include "altrep.h"
#include "tools.h"
#include "altrep_common_func.h"
#include "altrep_template.h"





using namespace Rcpp;
using std::map;
using std::pair;



R_altrep_class_t altrep_real_class;

R_altrep_class_t altrep_internal_real_class;
R_altrep_class_t altrep_internal_integer_class;

map<string, altClassKey> altrep_name_map;
map<altClassKey, altClassType> altrep_type_map;


map<altClassKey, SEXP> altrep_inspect_map;
map<altClassKey, SEXP> altrep_length_map;
map<altClassKey, SEXP> altrep_duplicate_map;
map<altClassKey, SEXP> altrep_coerce_map;
map<altClassKey, SEXP> altrep_serialize_map;
map<altClassKey, SEXP> altrep_unserialize_map;
map<altClassKey, SEXP> altrep_dataptr_map;
map<altClassKey, SEXP> altrep_dataptr_or_null_map;
map<altClassKey, SEXP> altrep_subset_map;
map<altClassKey, SEXP> altrep_get_element_map;
map<altClassKey, SEXP> altrep_region_map;




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








/*================================================================================================*/

//[[Rcpp::init]]
void initialAltrepClass(DllInfo* dll) {
	R_altrep_class_t alt_class;
	const char* class_name = "alt_real";
	alt_class = R_make_altreal_class(class_name, PACKAGE_NAME, dll);
	ALTREP_COMMOM_REGISTRATION(alt_class);
    R_set_altreal_Elt_method(alt_class, altrep_get_element<double>);
	R_set_altreal_Get_region_method(alt_class, numeric_region<double>);
	altrep_real_class = alt_class;

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


//This macro will be undefined after use
#define MATCH_TYPE(x) \
if (type_name == #x) {\
return(x);\
}
altClassType get_altrep_type(std::string type_name) {
	MATCH_TYPE(logical);
	MATCH_TYPE(integer);
	MATCH_TYPE(real);
}
#undef MATCH_TYPE

void register_class(string class_name, altClassType class_type) {

	std::hash<std::string> str_hash;
	//convert size_t into an R object and convert it back to make sure R has 
	//enough precision to store the key
	altClassKey key = as<altClassKey>(wrap(str_hash(class_name)));
	altrep_name_map.insert(pair<string,altClassKey>(class_name, key));
	altrep_type_map.insert(pair<altClassKey, altClassType>(key, class_type));
}

void delete_class(string class_name) {
	if (HAS_KEY(altrep_name_map, class_name)) {
		altClassKey key = altrep_name_map[class_name];
		altrep_name_map.erase(class_name);
		altrep_type_map.erase(key);
		altrep_inspect_map.erase(key);
		altrep_length_map.erase(key);
		altrep_duplicate_map.erase(key);
		altrep_coerce_map.erase(key);
		altrep_serialize_map.erase(key);
		altrep_unserialize_map.erase(key);
		altrep_dataptr_map.erase(key);
		altrep_dataptr_or_null_map.erase(key);
		altrep_subset_map.erase(key);
		altrep_get_element_map.erase(key);
		altrep_region_map.erase(key);
	}
}

void print_class(string class_name) {
	altClassKey key = altrep_name_map[class_name];
	Rprintf("Class name: %s\n", class_name.c_str());
	Rprintf("Class key: %d\n", key);
	Rprintf("Inspect: %s\n", HAS_KEY(altrep_inspect_map, key) ? "true" : "false");
	Rprintf("Length: %s\n", HAS_KEY(altrep_length_map, key) ? "true" : "false");
	Rprintf("Duplicate: %s\n", HAS_KEY(altrep_duplicate_map, key) ? "true" : "false");
	Rprintf("Coerce: %s\n", HAS_KEY(altrep_coerce_map, key) ? "true" : "false");
	Rprintf("Serialize: %s\n", HAS_KEY(altrep_serialize_map, key) ? "true" : "false");
	Rprintf("Unserialize: %s\n", HAS_KEY(altrep_unserialize_map, key) ? "true" : "false");
	Rprintf("DataPtr: %s\n", HAS_KEY(altrep_dataptr_map, key) ? "true" : "false");
	Rprintf("Dataptr_or_null: %s\n", HAS_KEY(altrep_dataptr_or_null_map, key) ? "true" : "false");
	Rprintf("Subset: %s\n", HAS_KEY(altrep_subset_map, key) ? "true" : "false");
	Rprintf("Get element: %s\n", HAS_KEY(altrep_get_element_map, key) ? "true" : "false");
	Rprintf("region: %s\n", HAS_KEY(altrep_region_map, key) ? "true" : "false");
}