#include "Rcpp.h"
#include <string>

#define DEBUG(x)

#define PACKAGE_NAME "AltWrapper"
#define PACKAGE_ENV_NAME "namespace:" PACKAGE_NAME
#define PACKAGE_NAMESPACE R_FindNamespace(Rf_mkString(PACKAGE_NAME))

//Macros to handle the map
#define HAS_KEY(map,key) map.find(key)!=map.end()
#define ERROR_WHEN_NOT_FIND_STR_KEY(map,key)\
if (map.find(key) == map.end()) errorHandle("Unable to find the class type: %s", key.c_str());

#define ERROR_WHEN_NOT_FIND_INT_KEY(map,key)\
if (map.find(key) == map.end()) errorHandle("Unable to find the key `%d` in the map `%s`", key,#map);

//To char*
#define SYMBOL_TO_CHAR(x) CHAR(PRINTNAME(x))
#define CHARSXP_TO_CHAR(x) CHAR(STRING_ELT(x, 0))


void errorHandle(std::string msg);
void errorHandle(const char* fmt, ...);
void warningHandle(std::string msg);
void warningHandle(const char* fmt, ...);
void messageHandle(std::string msg);
void messageHandle(const char* fmt, ...);



//Call a function with arguments
SEXP make_call(SEXP fun);
SEXP make_call(SEXP fun, SEXP x1);
SEXP make_call(SEXP fun, SEXP x1, SEXP x2);
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3);
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3, SEXP x4);
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3, SEXP x4, SEXP x5);