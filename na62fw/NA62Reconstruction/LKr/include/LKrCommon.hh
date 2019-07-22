#ifndef LKrCommon_H
#define LKrCommon_H 1

#include "TMath.h"
#include <fcntl.h>

#include "Riostream.h"
#include <string> 
#include <iostream>
using namespace std;

struct lkrpara_t {
  int APED;
  int BPED;
  int APUL;
  int BPUL;
  int MINPUL;
  int NUSEDSMPL;
  int SATLEVEL;
  int LKR_MINNDATA;
  int IWANTANA;
  int NDCP;
};

struct kafCDE_t {
  int KAFIHEA[128][128];
};

struct adcCDE_t {
  int CELLID;
  float PEAKENE;
  float PEAKTIME;
  int IQUALITY;
  int IFLAG;
};

struct cadCDE_t {
  int IXLKR[14000];
  int IYLKR[14000];
  long long IBENE[14000];
  long long IBADC[14000];
  int CALDEAD[14000];
  int ICLUS6[14000];
  int NCLUS6[14000][15];
  int ICLUS13[14000];
  float EPULSE[14000];
  float TPULSE[14000];
  int IFLAG[14000];
  int CALNCEL;
  int NSAMPL;
  float EFIRST_SAMPLE[14000];
  int LPLKRE1;
  int LPLKRA1;
};

struct cunCDE_t {
  float EHIGH[250];
  float THIGH[250];
  int INDHIGH[128][128];
  int IXHIGH[250];
  int IYHIGH[250];
  int ITHIGH[250];
  int IUSED[250];
  int NHIGH;
  int IPOINTLKR[128][128];
  float XCELL;
  float YCELL;
  float EDGE_X;
  float EDGE_Y;
  float EDGE_D;
  float XCELL_LKR[128];
  float YCELL_LKR[128];
};

struct deaCDE_t {
  int NDEAD;
  float XDEAD[900];
  float YDEAD[900];
};

struct corCDE_t {
  float LKRENERGYSCALE;
  float AXCORR[2];
  float BXCORR[2];
  float CXCORR[2];
  float DXCORR[2];
  float EXCORR[2];
  float FXCORR[2];
  float AYCORR[3];
  float BYCORR[3];
  float PAR[6];
  float PAR2[3];
  float RMSXPAR;
  float RMSYPAR;
  float ECHOLE[4];
  float ECOUT[4];
  float XPAR[5][7][6];
};

struct cluCDE_t {
  int NCLUS;
  float ECLUS[25];
  float ERAW[25];
  float XCLUS[25];
  float YCLUS[25];
  float TSEED[25];
  int IXSEED[25];
  int IYSEED[25];
  int ITSEED[25];
  float RMSX5[25];
  float RMSY5[25];
  int NCCLUS[25];
  bool DEAD33[25];
  int NBAD_CELL[25];
  int IBAD_CELL[25][10];
  bool DEAD55[25];
  float EARRAY[11][11][25];
  float TARRAY[11][11][25];
  int IFLARRAY[11][11][25];
  int IFLAGREC;
  float ENOISE[25];
  float SPACECORR[25];
  float ERCAL[25];
  float ECLUSF[25];
  float XCLUSF[25];
  float YCLUSF[25];
  float ENOISE7[25];
  float ESEED[25];
};

struct tcrCDE_t {
  int PULNENT;
  float PULSTEP;
  int PULNPEAK;
  float PULSCALE;
  float PBASE;
  float PULCONS[3000];
  float PULLINE[3000];  
};

struct ct0CDE_t {
  int TRIGTYPE;
  float TRIGGERDRIFTT0;
  float CELLT0[128][128];
};

struct rkeCDE_t {
  float CORRKE3[128][128];
};

struct clkCDE_t {
  float CLOCKPERIOD;
};

struct outCDE_t {
  float NCLUSTERS;
  float ETOTAL;
  float IRECFLAG;
  int IDCLUS[25];
  int NCELLS[25];
  int IDSEEDCELL[25];
  float ENERGY[25];
  float EENERGY[25];
  int STATFLAG[25];
  float XPOS[25];
  float YPOS[25];
  float RMSX[25];
  float RMSY[25];
  float TIME[25];
  float CHI2RMS[25];
  float TLATCELL[25];
  float DDEADCELL[25];
  float UENERGY[25];
  float E2SAMPALL[25];
  float E77CLUS[25];
  float SPACHARCORR[25];
  float ECORRKE3[25];
  float ENOIS77[25];
  float UTIME[25];
  int N77[25];
  int IDCELL77[50][25];
  int IFLCELL77[50][25];
  float ECELL77[50][25];
  float TCELL77[50][25];
  float ESEEDCELL[25];
};

struct LKRdigifilter {
  char FILENAME[255];
  char FILENAME2[255];
};

struct LKRdatatype {
  int DTYPE;
};

class LKrCommon 
{
  public:
    LKrCommon();
    static LKrCommon* GetInstance();
    void ResetEvent();

  private:
    static LKrCommon* fInstance;

  public:
    lkrpara_t *GetLKRPARA() { return flkrpara; };
    kafCDE_t *GetKAF() { return fkafCDE; };
    adcCDE_t *GetADC() { return fadcCDE; };
    cadCDE_t *GetCAD() { return fcadCDE; };
    cunCDE_t *GetCUN() { return fcunCDE; };
    deaCDE_t *GetDEA() { return fdeaCDE; };
    corCDE_t *GetCOR() { return fcorCDE; };
    cluCDE_t *GetCLU() { return fcluCDE; };
    tcrCDE_t *GetTCR() { return ftcrCDE; };
    ct0CDE_t *GetCT0() { return fct0CDE; };
    rkeCDE_t *GetRKE() { return frkeCDE; };
    clkCDE_t *GetCLK() { return fclkCDE; };
    outCDE_t *GetOUT() { return foutCDE; };

  private:
    lkrpara_t* flkrpara;
    kafCDE_t* fkafCDE;
    adcCDE_t* fadcCDE;
    cadCDE_t* fcadCDE;
    cunCDE_t* fcunCDE;
    deaCDE_t* fdeaCDE;   
    corCDE_t* fcorCDE;   
    cluCDE_t* fcluCDE;   
    tcrCDE_t* ftcrCDE;   
    ct0CDE_t* fct0CDE;   
    rkeCDE_t* frkeCDE;   
    clkCDE_t* fclkCDE;   
    outCDE_t* foutCDE;   
};

#endif
