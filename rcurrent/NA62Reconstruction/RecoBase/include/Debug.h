#ifdef DEBUG

#ifndef DGBLEVEL
#define DBGLEVEL 2
#endif

#include <string.h>
#define Str(x) #x
#define XStr(x) Str(x)

#define debug_cout(level, x) if(DBGLEVEL > level && (strstr(XStr(DEBUG),DBGTHIS) != 0 || strstr(XStr(DEBUG),DBGALL) != 0)) cout << x << endl
#define debug2_cout(level, x) if(DBGLEVEL > level && (strstr(XStr(DEBUG),DBGTHIS) != 0 || strstr(XStr(DEBUG),DBGALL) != 0)) cout << x
#else
#define debug_cout(level, x)
#define debug2_cout(level, x)
#endif


