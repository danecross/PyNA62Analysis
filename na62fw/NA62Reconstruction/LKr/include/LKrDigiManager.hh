#ifndef LKrDigiManager_H
#define LKrDigiManager_H 1

#include "TROOT.h"
#include "TClonesArray.h"
#include "TMath.h"
#include <fcntl.h>
#include "TLKrDigi.hh"

#include <iostream>
using namespace std;

class FADCEvent;
class LKrParameters;
class LKrGeometry;
class LKrCommon;

class LKrDigiManager
{
  public: 
    LKrDigiManager();
   ~LKrDigiManager();
    FADCEvent *ADCtoEnergy();
    Int_t CalRead();
    inline void ImportEvent(FADCEvent *event) {fFADCEventADC = event;};
    void Reset();

  private:
    FADCEvent *fFADCEventADC;
    FADCEvent *fFADCEventEnergy;
    LKrParameters *fPar;
    LKrGeometry *fGeo;
    LKrCommon *fCom;
    Double_t *fEnergySamples;
    Double_t fEnergyPeak;
    Double_t fPedRef;
    Int_t fBadGain[4];
    Double_t fOffset[4];
    Double_t fSteig[4];

  private: // Running conditions from config file
    Int_t fNSampPed;
    Int_t fNSampNoise;
    Int_t fSampTLow;
    Int_t fSampTHigh;
    Double_t fSeedECut1;
    Double_t fSeedECut2;

  private:
    Int_t CheckPed(TLKrDigi *);
    void CheckCal(Int_t ix, Int_t iy);
    void ResetCell();
    void ResetCommon();
};

#endif
