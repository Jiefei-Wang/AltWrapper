#include <map>
#include <string>
#include "R_ext/Altrep.h"

typedef size_t altClassKey;

enum altClassType { logical, integer, real };

extern SEXP ALTREP_CLASS_SPACE;
//The symbol is used for quick reference
extern SEXP ALTREP_SYMBOL_LIST;


SEXP get_valid_func_name();

altClassType get_altrep_enum_type_by_type_name(SEXP type_name);
altClassType get_altrep_enum_type_by_class_symbol(SEXP class_symbol_name);
R_altrep_class_t get_altrep_class(SEXP class_symbol_name);

SEXP get_alt_symbol(const char* name);
bool has_alt_class(SEXP class_symbol_name);
bool has_alt_function(SEXP class_symbol_name, SEXP function_symbol_name);
void register_alt_class(SEXP class_symbol_name, altClassType class_type, bool redefineWarning);
void register_alt_method(SEXP class_symbol_name, SEXP func_symbol_name, SEXP func, bool redefineWarning);
void delete_class(SEXP class_symbol_name);







