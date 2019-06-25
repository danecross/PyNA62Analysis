#include <string.h>

#define lkrdigifilter lkrdigifilter_
#define lkrdatatype lkrdatatype_
#define lkrpara lkrpara_
#define kafcde kafcde_
#define adccde adccde_
#define cadcde cadcde_
#define cuncde cuncde_
#define deacde deacde_
#define corcde corcde_
#define clucde clucde_
#define tcrcde tcrcde_
#define ct0cde ct0cde_
#define rkecde rkecde_
#define clkcde clkcde_
#define outcde outcde_
#define type_of_call

char lkrdigifilter[510];
int lkrdatatype[1];
int lkrpara[10];
int kafcde[16384];
float adccde[5];
float cadcde[364004];
float cuncde[34530];
float deacde[1801];
float corcde[248];
float clucde[9877];
float tcrcde[6005];
float ct0cde[16384];
float rkecde[16384];
float clkcde[1];
float outcde[5578];

void *lkr_common_address(const char* name) {
   if (!strcmp(name,"lkr_digifilter")) return lkrdigifilter;
   if (!strcmp(name,"lkr_datatype")) return lkrdatatype;
   if (!strcmp(name,"lkr_para")) return lkrpara;
   if (!strcmp(name,"kaf_cde")) return kafcde;
   if (!strcmp(name,"adc_cde")) return adccde;
   if (!strcmp(name,"cad_cde")) return cadcde;
   if (!strcmp(name,"cun_cde")) return cuncde;
   if (!strcmp(name,"dea_cde")) return deacde;
   if (!strcmp(name,"cor_cde")) return corcde;
   if (!strcmp(name,"clu_cde")) return clucde;
   if (!strcmp(name,"tcr_cde")) return tcrcde;
   if (!strcmp(name,"ct0_cde")) return ct0cde;
   if (!strcmp(name,"rke_cde")) return rkecde;
   if (!strcmp(name,"clk_cde")) return clkcde;
   if (!strcmp(name,"out_cde")) return outcde;
   return 0;
}
