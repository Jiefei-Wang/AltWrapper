#include <map>
#include <string>
#include "R_ext/Altrep.h"

extern SEXP ALTREP_REGISTRY_ENVIRONMENT;
//The symbol is used for quick reference
extern SEXP ALTREP_SYMBOL_LIST;


R_altrep_class_t get_altrep_class(SEXP class_type);

R_altrep_class_t get_altrep_internal_class(SEXP class_type);
int get_class_type_size(SEXP class_type);


SEXP get_alt_symbol(const char* name);






