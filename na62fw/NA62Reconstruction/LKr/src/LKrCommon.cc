#include "LKrCommon.hh"

extern "C"
{
  void* lkr_common_address(const char*);
}

LKrCommon* LKrCommon::fInstance = 0;

LKrCommon::LKrCommon(){
  flkrpara = static_cast<lkrpara_t *>(lkr_common_address("lkr_para"));
  fkafCDE =  static_cast<kafCDE_t *>(lkr_common_address("kaf_cde"));
  fadcCDE =  static_cast<adcCDE_t *>(lkr_common_address("adc_cde"));
  fcadCDE =  static_cast<cadCDE_t *>(lkr_common_address("cad_cde"));
  fcunCDE =  static_cast<cunCDE_t *>(lkr_common_address("cun_cde"));
  fdeaCDE =  static_cast<deaCDE_t *>(lkr_common_address("dea_cde"));
  fcorCDE =  static_cast<corCDE_t *>(lkr_common_address("cor_cde"));
  fcluCDE =  static_cast<cluCDE_t *>(lkr_common_address("clu_cde"));
  ftcrCDE =  static_cast<tcrCDE_t *>(lkr_common_address("tcr_cde"));
  fct0CDE =  static_cast<ct0CDE_t *>(lkr_common_address("ct0_cde"));
  frkeCDE =  static_cast<rkeCDE_t *>(lkr_common_address("rke_cde"));
  fclkCDE =  static_cast<clkCDE_t *>(lkr_common_address("clk_cde"));
  foutCDE =  static_cast<outCDE_t *>(lkr_common_address("out_cde"));
}

LKrCommon* LKrCommon::GetInstance(){
  if (fInstance==0) {fInstance = new LKrCommon();}
  return fInstance;
}

void LKrCommon::ResetEvent(){
  // cadCDE
  for (Int_t i=0; i<14000; i++){
    fcadCDE->IXLKR[i] = 0;
    fcadCDE->IYLKR[i] = 0;
    fcadCDE->IBENE[i] = 0;
    fcadCDE->IBADC[i] = 0;
    fcadCDE->CALDEAD[i] = 0;
    fcadCDE->ICLUS6[i] = 0;
    for (Int_t j=0; j<15; j++) fcadCDE->NCLUS6[i][j] = 0;
    fcadCDE->ICLUS13[i] = 0;
    fcadCDE->EPULSE[i] = 0;
    fcadCDE->TPULSE[i] = 0;
    fcadCDE->IFLAG[i] = 0;
    fcadCDE->EFIRST_SAMPLE[i] = 0;
  }
  fcadCDE->CALNCEL = 0;
  fcadCDE->NSAMPL = 0;
  fcadCDE->LPLKRE1 = 0;
  fcadCDE->LPLKRA1 = 0;

  // cunCDE
  for (Int_t i=0; i<250; i++){
    fcunCDE->EHIGH[i] = 0;
    fcunCDE->THIGH[i] = 0;
    fcunCDE->IXHIGH[i] = 0;
    fcunCDE->IYHIGH[i] = 0;
    fcunCDE->ITHIGH[i] = 0;
    fcunCDE->IUSED[i] = 0;
  }
  for (Int_t i=0; i<128; i++){
    for (Int_t j=0; j<128; j++){
      fcunCDE->INDHIGH[i][j] = 0;
      fcunCDE->IPOINTLKR[i][j] = 0;
    }
  }
  fcunCDE->NHIGH = 0;

  // deaCDE
  for (Int_t i=0; i<900; i++){
    fdeaCDE->XDEAD[i] = -9999;
    fdeaCDE->YDEAD[i] = -9999;
  }
  fdeaCDE->NDEAD = 0;

  // cluCDE
  for (Int_t i=0; i<25; i++){
    fcluCDE->NCLUS = 0;
    fcluCDE->ECLUS[i] = 0;
    fcluCDE->IFLAGREC = 0;
    fcluCDE->ERAW[i] = 0;
    fcluCDE->XCLUS[i] = -9999;
    fcluCDE->YCLUS[i] = -9999;
    fcluCDE->TSEED[i] = 0;
    fcluCDE->IXSEED[i] = 0;
    fcluCDE->IYSEED[i] = 0;
    fcluCDE->ITSEED[i] = 0;
    fcluCDE->RMSX5[i] = 0;
    fcluCDE->RMSY5[i] = 0;
    fcluCDE->NCCLUS[i] = 0;
    fcluCDE->DEAD33[i] = 0;
    fcluCDE->DEAD55[i] = 0;
    fcluCDE->NBAD_CELL[i] = 0;
    fcluCDE->ENOISE[i] = 0; 
    fcluCDE->SPACECORR[i] = 0;
    fcluCDE->ERCAL[i] = 0;
    fcluCDE->ECLUSF[i] = 0;
    fcluCDE->XCLUSF[i] = 0;
    fcluCDE->YCLUSF[i] = 0;
    fcluCDE->ENOISE7[i] = 0;
    for (Int_t j=0; j<10; j++) fcluCDE->IBAD_CELL[i][j] = 0;
    for (Int_t j=0; j<11; j++){
      for (Int_t k=0; k<11; k++){
        fcluCDE->EARRAY[k][j][i] = 0;
        fcluCDE->TARRAY[k][j][i] = 0;
        fcluCDE->IFLARRAY[k][j][i] = 0;
      } 
    }
    fcluCDE->ESEED[i] = 0;
  } 

  // ct0CDE
  fct0CDE->TRIGTYPE = 0;

  // outCDE
  for (Int_t i=0;i<25;i++){
    foutCDE->NCLUSTERS = 0;
    foutCDE->ETOTAL = 0;
    foutCDE->IRECFLAG = 0;
    foutCDE->IDCLUS[i] = 0;
    foutCDE->NCELLS[i] = 0;
    foutCDE->IDSEEDCELL[i] = 0;
    foutCDE->ENERGY[i] = 0;
    foutCDE->EENERGY[i] = 0;
    foutCDE->STATFLAG[i] = 0;
    foutCDE->XPOS[i] = 9999;
    foutCDE->YPOS[i] = 9999;
    foutCDE->RMSX[i] = 0;
    foutCDE->RMSY[i] = 0;
    foutCDE->TIME[i] = 0;
    foutCDE->CHI2RMS[i] = 99999;
    foutCDE->TLATCELL[i] = 0;
    foutCDE->DDEADCELL[i] = 0;
    foutCDE->UENERGY[i] = 0;
    foutCDE->E2SAMPALL[i] = 0;
    foutCDE->E77CLUS[i] = 0;
    foutCDE->SPACHARCORR[i] = 0;
    foutCDE->ECORRKE3[i] = 0;
    foutCDE->ENOIS77[i] = 0;
    foutCDE->UTIME[i] = 0;
    foutCDE->N77[i] = 0;
    for (Int_t j=0; j<50; j++){
      foutCDE->IDCELL77[j][i] = 0;
      foutCDE->IFLCELL77[j][i] = 0;
      foutCDE->ECELL77[j][i] = 0;
      foutCDE->TCELL77[j][i] = 0;
    }
    foutCDE->ESEEDCELL[i] = 0;
  }
}
