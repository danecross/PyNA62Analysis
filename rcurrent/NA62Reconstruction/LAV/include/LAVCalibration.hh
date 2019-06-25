#ifndef LAVCalibration_H
#define LAVCalibration_H 1

#define MAXBLOCKMAP 130000
#define MAXPARSPERTHRESHOLD 5

#include "iostream"
#include "TString.h"
#include "LAVGeometry.hh"

class LAVCalibration
{

public:

  LAVCalibration();
  static LAVCalibration* GetInstance();

// Mode for reading T0's

  Bool_t HasReadT0s(){return fReadLAVT0s;} 
  Double_t GetBlockT0(Int_t iBlock){if (iBlock >= 0 && iBlock < MAXBLOCKMAP) return fT0Corr[iBlock]; else return -1;}

// Mode for reading residual slewing corrections

  Bool_t HasReadResidualSlewingCorrections(){return fReadSlewPars;} 
  Double_t GetResidualSlewingCorrectionPar(Int_t iThreshold, Int_t iPar){if (iPar >= 0 && iPar<MAXPARSPERTHRESHOLD && (iThreshold==0 || iThreshold==1)) return fSlewingPars[iThreshold][iPar]; else return 0;}
  void AssignSlewingCorrections(Int_t); // Input the MC flag
  
private:

  static LAVCalibration* fInstance;

  LAVGeometry* fGeom;

  void DoT0ReadingFromFile(TString); 
  void DoT0ReadingFromDB(); //in future, will input server, port, password
  Bool_t fReadLAVT0s;
  Double_t fT0Corr[MAXBLOCKMAP];
  Bool_t fReadSlewPars;
  Double_t fSlewingPars[2][MAXPARSPERTHRESHOLD];
  Int_t fIsMC;
};
#endif
