#include <string.h>

#define cmcint cmcint_
#define cmcdouble cmcdouble_
#define type_of_call

int    cmcint[5*50];
double cmcdouble[11*50];

void *cmc_common_address(const char* name) {
   if (!strcmp(name,"cmc_int")) return cmcint;
   else if (!strcmp(name,"cmc_double")) return cmcdouble;
   return 0;
}
