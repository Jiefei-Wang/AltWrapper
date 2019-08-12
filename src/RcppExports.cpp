// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "AltWrapper_types.h"
#include <Rcpp.h>

using namespace Rcpp;

// C_ALTREP
bool C_ALTREP(SEXP x);
RcppExport SEXP _AltWrapper_C_ALTREP(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_ALTREP(x));
    return rcpp_result_gen;
END_RCPP
}
// C_get_alt_data1
SEXP C_get_alt_data1(SEXP x);
RcppExport SEXP _AltWrapper_C_get_alt_data1(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_get_alt_data1(x));
    return rcpp_result_gen;
END_RCPP
}
// C_get_alt_data2
SEXP C_get_alt_data2(SEXP x);
RcppExport SEXP _AltWrapper_C_get_alt_data2(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_get_alt_data2(x));
    return rcpp_result_gen;
END_RCPP
}
// C_set_alt_data1
void C_set_alt_data1(SEXP x, SEXP value);
RcppExport SEXP _AltWrapper_C_set_alt_data1(SEXP xSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type value(valueSEXP);
    C_set_alt_data1(x, value);
    return R_NilValue;
END_RCPP
}
// C_set_alt_data2
void C_set_alt_data2(SEXP x, SEXP value);
RcppExport SEXP _AltWrapper_C_set_alt_data2(SEXP xSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type value(valueSEXP);
    C_set_alt_data2(x, value);
    return R_NilValue;
END_RCPP
}
// C_duplicate_object
SEXP C_duplicate_object(SEXP x, SEXP shallow);
RcppExport SEXP _AltWrapper_C_duplicate_object(SEXP xSEXP, SEXP shallowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type shallow(shallowSEXP);
    rcpp_result_gen = Rcpp::wrap(C_duplicate_object(x, shallow));
    return rcpp_result_gen;
END_RCPP
}
// C_create_altrep
SEXP C_create_altrep(SEXP class_symbol_name, SEXP x, SEXP class_type, SEXP state, SEXP attrName, SEXP attributes);
RcppExport SEXP _AltWrapper_C_create_altrep(SEXP class_symbol_nameSEXP, SEXP xSEXP, SEXP class_typeSEXP, SEXP stateSEXP, SEXP attrNameSEXP, SEXP attributesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type class_symbol_name(class_symbol_nameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type class_type(class_typeSEXP);
    Rcpp::traits::input_parameter< SEXP >::type state(stateSEXP);
    Rcpp::traits::input_parameter< SEXP >::type attrName(attrNameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type attributes(attributesSEXP);
    rcpp_result_gen = Rcpp::wrap(C_create_altrep(class_symbol_name, x, class_type, state, attrName, attributes));
    return rcpp_result_gen;
END_RCPP
}
// C_create_internal_altrep
SEXP C_create_internal_altrep(SEXP class_type, R_xlen_t length);
RcppExport SEXP _AltWrapper_C_create_internal_altrep(SEXP class_typeSEXP, SEXP lengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type class_type(class_typeSEXP);
    Rcpp::traits::input_parameter< R_xlen_t >::type length(lengthSEXP);
    rcpp_result_gen = Rcpp::wrap(C_create_internal_altrep(class_type, length));
    return rcpp_result_gen;
END_RCPP
}
// C_getName
int C_getName(SEXP x);
RcppExport SEXP _AltWrapper_C_getName(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_getName(x));
    return rcpp_result_gen;
END_RCPP
}
// C_duplicate
SEXP C_duplicate(SEXP x, bool shallow);
RcppExport SEXP _AltWrapper_C_duplicate(SEXP xSEXP, SEXP shallowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type shallow(shallowSEXP);
    rcpp_result_gen = Rcpp::wrap(C_duplicate(x, shallow));
    return rcpp_result_gen;
END_RCPP
}
// C_initial_package
void C_initial_package(SEXP altrep_class_space, SEXP altrep_symbol_space);
RcppExport SEXP _AltWrapper_C_initial_package(SEXP altrep_class_spaceSEXP, SEXP altrep_symbol_spaceSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type altrep_class_space(altrep_class_spaceSEXP);
    Rcpp::traits::input_parameter< SEXP >::type altrep_symbol_space(altrep_symbol_spaceSEXP);
    C_initial_package(altrep_class_space, altrep_symbol_space);
    return R_NilValue;
END_RCPP
}
// C_performace_test1
SEXP C_performace_test1(SEXP a, R_xlen_t n);
RcppExport SEXP _AltWrapper_C_performace_test1(SEXP aSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< R_xlen_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(C_performace_test1(a, n));
    return rcpp_result_gen;
END_RCPP
}
// C_performace_test2
SEXP C_performace_test2(SEXP env, SEXP sym, R_xlen_t n);
RcppExport SEXP _AltWrapper_C_performace_test2(SEXP envSEXP, SEXP symSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type env(envSEXP);
    Rcpp::traits::input_parameter< SEXP >::type sym(symSEXP);
    Rcpp::traits::input_parameter< R_xlen_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(C_performace_test2(env, sym, n));
    return rcpp_result_gen;
END_RCPP
}
// C_test1
SEXP C_test1(SEXP f, SEXP x);
RcppExport SEXP _AltWrapper_C_test1(SEXP fSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type f(fSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_test1(f, x));
    return rcpp_result_gen;
END_RCPP
}
// C_test2
SEXP C_test2(SEXP expr, SEXP env);
RcppExport SEXP _AltWrapper_C_test2(SEXP exprSEXP, SEXP envSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type expr(exprSEXP);
    Rcpp::traits::input_parameter< SEXP >::type env(envSEXP);
    rcpp_result_gen = Rcpp::wrap(C_test2(expr, env));
    return rcpp_result_gen;
END_RCPP
}
// C_test3
SEXP C_test3(SEXP f, SEXP x);
RcppExport SEXP _AltWrapper_C_test3(SEXP fSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type f(fSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_test3(f, x));
    return rcpp_result_gen;
END_RCPP
}
// C_test4
void C_test4(SEXP x);
RcppExport SEXP _AltWrapper_C_test4(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    C_test4(x);
    return R_NilValue;
END_RCPP
}
// C_test5
void C_test5(SEXP x);
RcppExport SEXP _AltWrapper_C_test5(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    C_test5(x);
    return R_NilValue;
END_RCPP
}
// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP _AltWrapper_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello_world());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_AltWrapper_C_ALTREP", (DL_FUNC) &_AltWrapper_C_ALTREP, 1},
    {"_AltWrapper_C_get_alt_data1", (DL_FUNC) &_AltWrapper_C_get_alt_data1, 1},
    {"_AltWrapper_C_get_alt_data2", (DL_FUNC) &_AltWrapper_C_get_alt_data2, 1},
    {"_AltWrapper_C_set_alt_data1", (DL_FUNC) &_AltWrapper_C_set_alt_data1, 2},
    {"_AltWrapper_C_set_alt_data2", (DL_FUNC) &_AltWrapper_C_set_alt_data2, 2},
    {"_AltWrapper_C_duplicate_object", (DL_FUNC) &_AltWrapper_C_duplicate_object, 2},
    {"_AltWrapper_C_create_altrep", (DL_FUNC) &_AltWrapper_C_create_altrep, 6},
    {"_AltWrapper_C_create_internal_altrep", (DL_FUNC) &_AltWrapper_C_create_internal_altrep, 2},
    {"_AltWrapper_C_getName", (DL_FUNC) &_AltWrapper_C_getName, 1},
    {"_AltWrapper_C_duplicate", (DL_FUNC) &_AltWrapper_C_duplicate, 2},
    {"_AltWrapper_C_initial_package", (DL_FUNC) &_AltWrapper_C_initial_package, 2},
    {"_AltWrapper_C_performace_test1", (DL_FUNC) &_AltWrapper_C_performace_test1, 2},
    {"_AltWrapper_C_performace_test2", (DL_FUNC) &_AltWrapper_C_performace_test2, 3},
    {"_AltWrapper_C_test1", (DL_FUNC) &_AltWrapper_C_test1, 2},
    {"_AltWrapper_C_test2", (DL_FUNC) &_AltWrapper_C_test2, 2},
    {"_AltWrapper_C_test3", (DL_FUNC) &_AltWrapper_C_test3, 2},
    {"_AltWrapper_C_test4", (DL_FUNC) &_AltWrapper_C_test4, 1},
    {"_AltWrapper_C_test5", (DL_FUNC) &_AltWrapper_C_test5, 1},
    {"_AltWrapper_rcpp_hello_world", (DL_FUNC) &_AltWrapper_rcpp_hello_world, 0},
    {NULL, NULL, 0}
};

void init_altrep_dispatcher(DllInfo* dll);
void init_altrep_internal_class(DllInfo* dll);
RcppExport void R_init_AltWrapper(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    init_altrep_dispatcher(dll);
    init_altrep_internal_class(dll);
}
