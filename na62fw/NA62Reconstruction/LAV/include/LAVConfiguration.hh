#ifndef LAVConfiguration_H
#define LAVConfiguration_H 1

#define MAXBLOCKMAP 130000

#include "iostream"
#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;
#include "LAVGeometry.hh"

class LAVConfiguration
{

public:

  LAVConfiguration();
  static LAVConfiguration* GetInstance();

// Thresholds and hysteresis

  Bool_t HasReadConfigurationInfos(){return fReadConfigurationInfos;} 

  Double_t GetBlockThresholdHigh(Int_t iBlock){if (iBlock >= 0 && iBlock < MAXBLOCKMAP) return fHighThreshold[iBlock]; else return -1;}
  Double_t GetBlockThresholdLow(Int_t iBlock){if (iBlock >= 0 && iBlock < MAXBLOCKMAP) return fLowThreshold[iBlock]; else return -1;}
  Double_t GetBlockHysteresis(Int_t iBlock){if (iBlock >= 0 && iBlock < MAXBLOCKMAP) return fHysteresis[iBlock]; else return -1;}

private:
  LAVGeometry* fGeom;
  static LAVConfiguration* fInstance;

  void DoThresholdReadingFromFile(TString);
  void DoThresholdReadingFromDB(); // in future, the method interface will include server, port, access info, e.g. (TString,Int_t,TString)
  Bool_t fReadConfigurationInfos;

  Double_t fLowThreshold[MAXBLOCKMAP];
  Double_t fHighThreshold[MAXBLOCKMAP];
  Double_t fHysteresis[MAXBLOCKMAP];
};
#endif
