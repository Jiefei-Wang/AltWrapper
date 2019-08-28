#include <string>
using std::string;

#define PACKAGE_NAME "AltWrapper"
#define PACKAGE_ENV_NAME "namespace:" PACKAGE_NAME
#define PACKAGE_NAMESPACE R_FindNamespace(Rf_mkString(PACKAGE_NAME))
#define DEBUG(x) x

#define HAS_KEY(map,key) map.find(key)!=map.end()
#define ERROR_WHEN_NOT_FIND_STR_KEY(map,key)\
if (map.find(key) == map.end()) errorHandle("Unable to find the class type: %s", key.c_str());

#define ERROR_WHEN_NOT_FIND_INT_KEY(map,key)\
if (map.find(key) == map.end()) errorHandle("Unable to find the key `%d` in the map `%s`", key,#map);


#define SYMBOL_TO_CHAR(x) CHAR(PRINTNAME(x))
#define CHARSXP_TO_CHAR(x) CHAR(STRING_ELT(x, 0))

#define ULLong unsigned long long


void errorHandle(std::string msg);
void errorHandle(const char* fmt, ...);
void warningHandle(std::string msg);
void warningHandle(const char* fmt, ...);

void messageHandle(std::string msg);
void messageHandle(const char* fmt, ...);



