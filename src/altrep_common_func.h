#include "Rcpp.h"

Rboolean altrep_inspect(SEXP x, int pre, int deep, int pvec,
	void (*inspect_subtree)(SEXP, int, int, int));

R_xlen_t altrep_length(SEXP x);
void* altrep_dataptr(SEXP x, Rboolean writeable);
const void* altrep_dataptr_or_null(SEXP x);
SEXP altrep_coerce(SEXP x, int type);
SEXP altrep_duplicate(SEXP x, Rboolean deep);
SEXP altrep_serialize_state(SEXP x);
SEXP altrep_unserialize(SEXP R_class, SEXP state);
SEXP altrep_subset(SEXP x, SEXP indx, SEXP call);



R_xlen_t altrep_internal_length(SEXP x);
void* altrep_internal_dataptr(SEXP x, Rboolean writeable);
SEXP altrep_internal_duplicate(SEXP x, Rboolean deep);