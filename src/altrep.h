#include <Rcpp.h>
#include "R_ext/Altrep.h"

//Declare the global variables

//a list of R's symbol
#define ALTREP_SYMBOLS \
X(0,inspect)\
X(1,getLength)\
X(2,getDataptr)\
X(3,getDataptrOrNull)\
X(4,getSubset)\
X(5,getElement)\
X(6,getRegion)\
X(7,duplicate)\
X(8,coerce)\
X(9,serialize)\
X(10,unserialize)\
X(11,isSorted)\
X(12,noNA)\
X(13,sum)\
X(14,min)\
X(15,max)


//The environment that stores the ALTREP class information
//All registered ALTREP classes will be here
extern SEXP ALTREP_REGISTRY_ENVIRONMENT;

//Dispacher altrep class
extern R_altrep_class_t altrep_raw_class;
extern R_altrep_class_t altrep_logical_class;
extern R_altrep_class_t altrep_integer_class;
extern R_altrep_class_t altrep_real_class;
//Internal altrep class
//A wrapper for the pointer type
extern R_altrep_class_t altrep_internal_raw_class;
extern R_altrep_class_t altrep_internal_logical_class;
extern R_altrep_class_t altrep_internal_integer_class;
extern R_altrep_class_t altrep_internal_real_class;

//Declare each R's symbol with _symbol postfix
#define X(i,func_name) \
extern const int func_name##_index;
ALTREP_SYMBOLS
#undef X


/* 
	Get the dispacher ALTREP class object by the class name
	The name must be a STRSXP
*/
R_altrep_class_t get_altrep_class(SEXP class_type);
/*
	Get the internal ALTREP class object by the class name(Internal use only)
	The internal ALTREP is a wrapper of the pointer, when it is passed as an argument to
	a function, its value can be changed and propagated to the ALTREP outside the function.
*/
R_altrep_class_t get_altrep_internal_class(SEXP class_type);

// The element size of a vector that is of the class type
int get_class_type_size(SEXP class_type);



/*
Get the symbol in ALTREP_SYMBOLS:
X(0,inspect)\
X(1,getLength)\
X(2,getDataptr)\
X(3,getDataptrOrNull)\
X(4,getSubset)\
X(5,getElement)\
X(6,getRegion)\
X(7,duplicate)\
X(8,coerce)\
X(9,serialize)\
X(10,unserialize)\
X(11,isSorted)\
X(12,noNA)\
X(13,sum)\
X(14,min)\
X(15,max)\
X(40, classType)\
X(41, functionSpace)\
X(42, classSettings)
*/



//SEXP get_alt_symbol(const char* name);







