#include <map>
#include <string>
#include "R_ext/Altrep.h"

typedef size_t altClassKey;


#define GET_ALT_DATA(x)  R_altrep_data1(x)
#define GET_ALT_ATTR(x,i) VECTOR_ELT(R_altrep_data2(x),i)
#define SET_ALT_ATTR(x,i,value) SET_VECTOR_ELT(R_altrep_data2(x),i,value)
#define GET_ALT_CLASS_NAME(x) GET_ALT_ATTR(x,0)
#define GET_ALT_CLASS_KEY(x) GET_ALT_ATTR(x,1)

#define SET_ALT_CLASS_NAME(x,value) SET_ALT_ATTR(x,0,value)
#define SET_ALT_CLASS_KEY(x,value) SET_ALT_ATTR(x,1,value)

enum altClassType { logical, integer, real };

extern std::map<std::string, altClassKey> altrep_name_map;
extern std::map<altClassKey, altClassType> altrep_type_map;
extern std::map<altClassKey, SEXP> altrep_inspect_map;
extern std::map<altClassKey, SEXP> altrep_length_map;
extern std::map<altClassKey, SEXP> altrep_duplicate_map;
extern std::map<altClassKey, SEXP> altrep_coerce_map;
extern std::map<altClassKey, SEXP> altrep_serialize_map;
extern std::map<altClassKey, SEXP> altrep_unserialize_map;
extern std::map<altClassKey, SEXP> altrep_dataptr_map;
extern std::map<altClassKey, SEXP> altrep_dataptr_or_null_map;
extern std::map<altClassKey, SEXP> altrep_subset_map;
extern std::map<altClassKey, SEXP> altrep_get_element_map;
extern std::map<altClassKey, SEXP> altrep_region_map;

extern R_altrep_class_t altrep_real_class;

extern R_altrep_class_t altrep_internal_real_class;
extern R_altrep_class_t altrep_internal_integer_class;

altClassType get_altrep_type(std::string);
void register_class(std::string class_name, altClassType class_type);
void delete_class(std::string class_name);
void print_class(std::string class_name);







