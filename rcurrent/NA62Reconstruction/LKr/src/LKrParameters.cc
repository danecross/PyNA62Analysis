#include "LKrParameters.hh"
#include "TMath.h"
#include "LKrCommon.hh"
#include "TVEvent.hh"
#include "NA62ConditionsService.hh"
#include <sstream>

#define filtini filtini_
#define type_of_call
extern "C"
{
  void type_of_call filtini();

  // Common
  void* lkr_common_address(const char*);
}

/// \class LKrParameters 
/// \Brief
/// LKr parameters. 
/// \EndBrief
/// 
/// \Detailed
/// This class stores the lkr reconstruction parameters. 
/// \EndDetailed

LKrParameters* LKrParameters::fInstance = 0;

LKrParameters::LKrParameters() :
  fIsRawData(false),
  fDigiFilterPreProcess(0),
  fNSamplePedestals(3),
  fNSampleNoise(1),
  fSampleTimeLow(1),
  fSampleTimeHigh(0),
  fSeedEnergyCut(0),
  fSeedECutRatio(0),
  fEnergyScale(0),
  fTimePulNent(0),
  fTimePulStep(.0),
  fTimePulNpeak(0),
  fTimePulScale(.0),
  fTimePBase(.0),
  fZSAlgorithm(0),
  fOutputHits(0),
  fPedestalsEvaluation(0),
  fLKRdigifilter(nullptr)
{ 
}

LKrParameters * LKrParameters::GetInstance()
{
/// \return LKrParameters::fInstance.
/// \EndMemberDescr

  if (fInstance==0) {fInstance = new LKrParameters();}
  return fInstance;
}

void LKrParameters::Fill(LKrReconstruction* LKrReco){

  // Reset variables
  for (Int_t ix=0; ix<128; ix++)
  {
    for (Int_t iy=0; iy<128; iy++) 
    {
      fPedStat[ix][iy]  = 99.;
      fPedSigma[ix][iy] = 0.;
      fPedRef[ix][iy]   = 0.;
      fCalStat[ix][iy]  = 0.;
      fCellSta[ix][iy]  = 0.;
      fCellKe3[ix][iy]  = 0.;
      fCellT0[ix][iy]   = 0.;
      for (Int_t ig=0; ig<4; ig++)
      {
        fCalOffset[ix][iy][ig] = 0.;
        fCalSteig[ix][iy][ig] = 0.;
      }
    }
  }

  // Read pedestal status
  Int_t idx,idy;
  Double_t flag[128][128];
  string line1, line2, line3, line4;

  // Read pedestal status and pedestal reference (1 gain)
  Bool_t NewFormat = true;
  LKrChannelID* LKrCh = new LKrChannelID();
  if (NA62ConditionsService::GetInstance()->Open(fPedFileName)==kSuccess){
    Int_t NReadLines=0;
    TString Line;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fPedFileName))) {
      if (Line.BeginsWith("#")) continue;
      if (Line.BeginsWith("CHAPEDES")) {
        NewFormat = 0; //old format
        continue;
      }
      if (Line.BeginsWith("*")) continue; //old format compliant
      Int_t ROCh = -1, GeoCh = -1, Status = -1;
      Double_t PedRef = -1., PedSigma = -1.;
      stringstream ss;
      ss << Line;
      if(NewFormat) { //new format
        ss >> ROCh >> GeoCh >> Status >> PedRef >> PedSigma;
      }
      else { //old format, to be removed eventually
        ss >> idx >> idy >> Status >> PedRef >> PedSigma;
        GeoCh = idx*1000+idy;
      }
      LKrCh->DecodeChannelID(GeoCh);
      NReadLines++;
      if(LKrCh->GetXCellID()<0 || LKrCh->GetYCellID()<0) continue;
      fPedStat[LKrCh->GetXCellID()][LKrCh->GetYCellID()] = Status;
      fPedRef[LKrCh->GetXCellID()][LKrCh->GetYCellID()] = PedRef;
      fPedSigma[LKrCh->GetXCellID()][LKrCh->GetYCellID()] = PedSigma;
      if(NA62ConditionsService::GetInstance()->Get(fPedFileName).eof()) break;
    }
    NA62ConditionsService::GetInstance()->Close(fPedFileName);
    if(NReadLines!=16384) LKrReco->Exception(Form("Bad pedestal file '%s'!",fPedFileName.Data()));
  }
  delete LKrCh;

  if(NA62ConditionsService::GetInstance()->Open(fSlopeFileName)==kSuccess){
    getline(NA62ConditionsService::GetInstance()->Get(fSlopeFileName), line1);
    getline(NA62ConditionsService::GetInstance()->Get(fSlopeFileName), line2);
    getline(NA62ConditionsService::GetInstance()->Get(fSlopeFileName), line3);
    getline(NA62ConditionsService::GetInstance()->Get(fSlopeFileName), line4);
    Int_t NReadLines=0;
    while(!NA62ConditionsService::GetInstance()->Get(fSlopeFileName).eof()) { 
      NA62ConditionsService::GetInstance()->Get(fSlopeFileName) >> idx >> idy;
      NA62ConditionsService::GetInstance()->Get(fSlopeFileName) >> flag[idx][idy];
      NA62ConditionsService::GetInstance()->Get(fSlopeFileName) >> fCalSteig[idx][idy][0];
      fCalStat[idx][idy]=flag[idx][idy];
      if(NA62ConditionsService::GetInstance()->Get(fSlopeFileName).eof()) break;
      NReadLines++;
    }
    NA62ConditionsService::GetInstance()->Close(fSlopeFileName);
    if(NReadLines!=16384) LKrReco->Exception(Form("Bad slope file '%s'!",fSlopeFileName.Data()));
  }

  for(Int_t ix = 0; ix < 128; ix++) {
    for(Int_t iy = 0; iy < 128; iy++) {
      if(flag[ix][iy] == 4096) fCalStat[ix][iy] = 0;
      if(fPedStat[ix][iy] == 99 && flag[ix][iy] == 99) { //not instrumented cell
        fCellSta[ix][iy] = 4096;
        fCalStat[ix][iy] = 0.;
      }
      else if(fPedStat[ix][iy] > 0 || (flag[ix][iy] > 9 && flag[ix][iy] < 1000)) {
        fCellSta[ix][iy] = 2048;
      }
      else {
        fCalStat[ix][iy] = 0.;
        fCellSta[ix][iy] = 4;
      }
    }
  }

  // Read ke3 calibration constans
  if(NA62ConditionsService::GetInstance()->Open(fKe3CorrFileName)==kSuccess){
    while(!NA62ConditionsService::GetInstance()->Get(fKe3CorrFileName).eof()) {
      NA62ConditionsService::GetInstance()->Get(fKe3CorrFileName) >> idx >> idy;
      NA62ConditionsService::GetInstance()->Get(fKe3CorrFileName) >> fCellKe3[idx][idy];
    }
    NA62ConditionsService::GetInstance()->Close(fKe3CorrFileName);
  }

  // Read t0
  if (fIsRawData && LKrReco->GetEnableT0()) {
    if (NA62ConditionsService::GetInstance()->Open(fT0FileName)==kSuccess) {
      TString Line;
      while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fT0FileName))) {
        if (Line.BeginsWith("#")) continue;
        TObjArray *l = Line.Tokenize(" ");
        Int_t     ch = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
        Int_t     ix = ch/128;
        Int_t     iy = ch%128;
        Double_t  t0 = static_cast<TObjString*>(l->At(2))->GetString().Atof();
        if(fabs(t0)>999) t0 = 0.0; // -999.999: masked channel, +999.999: failed to compute T0
        if(0<=ix && ix<128 && 0<=iy && iy<128) fCellT0[ix][iy] = t0;
        delete l;
      }
      NA62ConditionsService::GetInstance()->Close(fT0FileName);
    }
  }

  // Zero calibration offsets
  for(Int_t idx = 0; idx < 128; idx++) {
    for(Int_t idy = 0; idy < 128; idy++) {
      fCalOffset[idx][idy][0] = 0.;
    }
  }

  // Fill relevant common
  FillCommon(LKrReco);
  // Read Digital Filter constant and fill the common (1 gain)  
  FillDigiFilter(NA62ConditionsService::GetInstance()->GetFullPath(fDigFiltConstFileName).Data(),NA62ConditionsService::GetInstance()->GetFullPath(fRefShapeFileName).Data());
  // Read and fill common for cluster correction 
  FillClusCorr();
}

void LKrParameters::DefineDataType(bool type) {
  LKRdatatype *datatype = static_cast<LKRdatatype *>( lkr_common_address("lkr_datatype"));
  datatype->DTYPE = (int)type;
  fIsRawData = type;
}

void LKrParameters::FillCommon(LKrReconstruction * LKrReco){
  LKrCommon *fCom = LKrCommon::GetInstance();
  fCom->GetLKRPARA()->APED = fDecodingParam[0];
  fCom->GetLKRPARA()->BPED = fDecodingParam[1];
  fCom->GetLKRPARA()->APUL = fDecodingParam[2];
  fCom->GetLKRPARA()->BPUL = fDecodingParam[3];
  fCom->GetLKRPARA()->MINPUL = fDecodingParam[4];
  fCom->GetLKRPARA()->NUSEDSMPL = fDecodingParam[5];
  fCom->GetLKRPARA()->SATLEVEL = fDecodingParam[6];
  fCom->GetLKRPARA()->LKR_MINNDATA = fDecodingParam[7];
  fCom->GetLKRPARA()->IWANTANA = fDecodingParam[8];
  fCom->GetLKRPARA()->NDCP = fDecodingParam[9];
  for (Int_t ix=0; ix<128; ix++){
    for (Int_t iy=0; iy<128; iy++){
      fCom->GetKAF()->KAFIHEA[iy][ix] = fCellSta[ix][iy];
      fCom->GetRKE()->CORRKE3[iy][ix] = (float)fCellKe3[ix][iy];   // PAY ATTENTION: ix and iy inverted since the ke3list is passed to fortran code (clus1.f) via rkeCDE 
      fCom->GetCT0()->CELLT0[iy][ix] = -((float)fCellT0[ix][iy]+LKrReco->GetT0Correction(ix*1000+iy,0));   // PAY ATTENTION: ix and iy inverted since the ke3list is passed to fortran code (clus1.f) via ct0CDE
    }
  }
  fCom->GetCLK()->CLOCKPERIOD = ClockPeriod;
}

void LKrParameters::SetTriggerType(UInt_t TrigType){
  LKrCommon *fCom = LKrCommon::GetInstance();
  fCom->GetCT0()->TRIGTYPE = TrigType&0xff;
}

void LKrParameters::SetTriggerDriftT0(Double_t DriftT0){
  LKrCommon *fCom = LKrCommon::GetInstance();
  fCom->GetCT0()->TRIGGERDRIFTT0 = DriftT0;
}

void LKrParameters::FillDigiFilter(const char *datacardname, const char *datacardname2)
{
  fLKRdigifilter = static_cast<LKRdigifilter *>( lkr_common_address("lkr_digifilter"));
  int size = strlen(datacardname);
  for (int j=0; j<size; j++) fLKRdigifilter->FILENAME[j] = datacardname[j];
  size = strlen(datacardname2);
  for (int j=0; j<size; j++) fLKRdigifilter->FILENAME2[j] = datacardname2[j];
  filtini();
}

void LKrParameters::FillClusCorr()
{
  // Cluster energy correction
  LKrCommon *fCom = LKrCommon::GetInstance();
  fCom->GetCOR()->LKRENERGYSCALE = (float)fEnergyScale;
  fCom->GetCOR()->AXCORR[0] = (float)fClusterXCorr[0];
  fCom->GetCOR()->AXCORR[1] = (float)fClusterXCorr[1];
  fCom->GetCOR()->BXCORR[0] = (float)fClusterXCorr[2];
  fCom->GetCOR()->BXCORR[1] = (float)fClusterXCorr[3];
  fCom->GetCOR()->CXCORR[0] = (float)fClusterXCorr[4];
  fCom->GetCOR()->CXCORR[1] = (float)fClusterXCorr[5];
  fCom->GetCOR()->DXCORR[0] = (float)fClusterXCorr[6];
  fCom->GetCOR()->DXCORR[1] = (float)fClusterXCorr[7];
  fCom->GetCOR()->EXCORR[0] = (float)fClusterXCorr[8];
  fCom->GetCOR()->EXCORR[1] = (float)fClusterXCorr[9];
  fCom->GetCOR()->FXCORR[0] = (float)fClusterXCorr[10];
  fCom->GetCOR()->FXCORR[1] = (float)fClusterXCorr[11];
  fCom->GetCOR()->AYCORR[0] = (float)fClusterYCorr[0];
  fCom->GetCOR()->AYCORR[1] = (float)fClusterYCorr[1];
  fCom->GetCOR()->AYCORR[2] = (float)fClusterYCorr[2];
  fCom->GetCOR()->BYCORR[0] = (float)fClusterYCorr[3];
  fCom->GetCOR()->BYCORR[1] = (float)fClusterYCorr[4];
  fCom->GetCOR()->BYCORR[2] = (float)fClusterYCorr[5];
  for (Int_t j=0; j<6; j++) fCom->GetCOR()->PAR[j] = (float)fClusterEvsXCorr[j];
  for (Int_t j=0; j<3; j++) fCom->GetCOR()->PAR2[j] = (float)fClusterEvsYCorr[j];
  fCom->GetCOR()->RMSXPAR = (float)fClusterRMSCorr[0];
  fCom->GetCOR()->RMSYPAR = (float)fClusterRMSCorr[1];
  for (Int_t j=0; j<4; j++) fCom->GetCOR()->ECHOLE[j] = (float)fClusterHoleCorr[j];
  for (Int_t j=0; j<4; j++) fCom->GetCOR()->ECOUT[j] = (float)fClusterOutCorr[j];

  // Parameters for cluster time reconstruction (used in tfrac.f from pulsesep.f)
  Int_t iline = 0;
  if(NA62ConditionsService::GetInstance()->Open(fPulConsFileName)==kSuccess){
    while(NA62ConditionsService::GetInstance()->Get(fPulConsFileName) >> fTimePulCons[iline]) iline++;
    NA62ConditionsService::GetInstance()->Close(fPulConsFileName);
  }

  iline = 0;
  if(NA62ConditionsService::GetInstance()->Open(fPulLineFileName)==kSuccess){
    while(NA62ConditionsService::GetInstance()->Get(fPulLineFileName) >> fTimePulLine[iline]) iline++;
    NA62ConditionsService::GetInstance()->Close(fPulLineFileName);
  }
  fCom->GetTCR()->PULNENT = fTimePulNent;
  fCom->GetTCR()->PULSTEP = (float)fTimePulStep;
  fCom->GetTCR()->PULNPEAK = fTimePulNpeak;
  fCom->GetTCR()->PULSCALE = (float)fTimePulScale;
  fCom->GetTCR()->PBASE = (float)fTimePBase;
  for (Int_t j=0; j<3000; j++){
    fCom->GetTCR()->PULCONS[j] = (float)fTimePulCons[j];
    fCom->GetTCR()->PULLINE[j] = (float)fTimePulLine[j];
  }
}

Bool_t LKrParameters::IsDeadCell(Int_t ix, Int_t iy)
{
  if (((Int_t)fCellSta[ix][iy]>>11)&1) return true;
  //if (((Int_t)fCellSta[ix][iy]>>12)&1) return true; //peripheral cells
  if (((Int_t)fCellSta[ix][iy]>>13)&1) return true;
  if (((Int_t)fCellSta[ix][iy]>>4)&1) return true;
  if ((Int_t)fCellSta[ix][iy]==0) return true;
  return false;
}
