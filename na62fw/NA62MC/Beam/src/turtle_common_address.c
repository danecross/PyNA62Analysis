#include <string.h>

#define turtleint turtleint_ 
#define turtledouble turtledouble_
#define turtlechar turtlechar_
#define bloc1 bloc1_
#define bloc2 bloc2_
#define bloc3 bloc3_
#define bloc4 bloc4_
#define bloc5 bloc5_
#define bloc6 bloc6_
#define bloc7 bloc7_
#define bloc8 bloc8_
#define bloc9 bloc9_
#define bloc10 bloc10_
#define bloc11 bloc11_
#define bloc12 bloc12_
#define bloc13 bloc13_
#define bloc14 bloc14_
#define bloc15 bloc15_
#define bloc16 bloc16_
#define bloc17 bloc17_
#define bloc18 bloc18_
#define bloc41 bloc41_
#define bloc42 bloc42_
#define bloc43 bloc43_
#define type_of_call

int    turtleint[1];
double turtledouble[6];
double bloc1[3003];
double bloc2[22];
int bloc3[2];
double bloc4[9];
double bloc5[5];
double bloc6[3];
double bloc7[800];
double bloc8[100000];
double bloc9[4];
double bloc10[7];
double bloc11[5];
double bloc12[636];
double bloc13[31];
double bloc14[62];
double bloc15[21];
double bloc16[1503];
double bloc17[9];
double bloc18[121];
double bloc41[88];
int bloc42[56];
double bloc43[84];
char turtlechar[100];

void *turtle_common_address(const char* name) {
   if (!strcmp(name,"turtle_int")) return turtleint;
   else if (!strcmp(name,"turtle_double")) return turtledouble;
   else if (!strcmp(name,"turtle_char")) return turtlechar;
   return 0;
}
