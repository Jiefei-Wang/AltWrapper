#include <string>
using std::string;

#define PACKAGE_NAME "SharedObject"
#define PACKAGE_ENV_NAME "package:" PACKAGE_NAME
#define DEBUG(x)

#define HAS_KEY(map,key) map.find(key)!=map.end()
#define ERROR_WHEN_NOT_FIND_STR_KEY(map,key)\
if (map.find(key) == map.end()) errorHandle("Unable to find the class type: %s", key.c_str());

#define ERROR_WHEN_NOT_FIND_INT_KEY(map,key)\
if (map.find(key) == map.end()) errorHandle("Unable to find the key `%d` in the map `%s`", key,#map);

#define SEXP_TO_CHAR(x) as<string>(x).c_str()

#define SYMBOL_TO_CHAR(x) CHAR(PRINTNAME(x))





void errorHandle(std::string msg);
void errorHandle(const char* fmt, ...);
void warningHandle(std::string msg);
void warningHandle(const char* fmt, ...);

void messageHandle(std::string msg);
void messageHandle(const char* fmt, ...);



