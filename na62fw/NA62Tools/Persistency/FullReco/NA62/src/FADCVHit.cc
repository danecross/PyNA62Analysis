// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-03-30
//            Evelina Gersabeck (Evelina.Gersabeck@cern.ch)
//
// --------------------------------------------------------------
#include "FADCVHit.hh"
ClassImp(FADCVHit)
  
FADCVHit::FADCVHit() :
  TVDigi(),
  fPeakEnergy(0),
  fPeakTime(-1.e28),
  fADCPeakEnergy(0),
  fADCPeakTime(0),
  fQuality(0),
  fFlags(0),
  fNSamples(0),
  fNMaxSamples(8),
  fGain(0)
{
  fSamples.Set(fNMaxSamples);
}

FADCVHit::FADCVHit(Int_t iCh) :
  TVDigi(iCh),
  fPeakEnergy(0),
  fPeakTime(-1.e28),
  fADCPeakEnergy(0),
  fADCPeakTime(0),
  fQuality(0),
  fFlags(0),
  fNSamples(0),
  fNMaxSamples(8),
  fGain(0)
{
  fSamples.Set(fNMaxSamples);
}

FADCVHit::FADCVHit(TVHit* MCHit) :
  TVDigi(MCHit),
  fPeakEnergy(0),
  fPeakTime(-1.e28),
  fADCPeakEnergy(0),
  fADCPeakTime(0),
  fQuality(0),
  fFlags(0),
  fNSamples(0),
  fNMaxSamples(8),
  fGain(0)
{
  fSamples.Set(fNMaxSamples);
}

void FADCVHit::Clear(Option_t * option) {
  TVDigi::Clear(option);
  fSamples.Reset();
  fNSamples = 0;
  fPeakEnergy = 0;
  fPeakTime = -1.e28;
  fADCPeakEnergy = 0;
  fADCPeakTime = 0;
  fQuality = 0;
  fFlags = 0;  
  fGain = 0;  
}

void FADCVHit::AddSample(Double_t Value){
  if (fNSamples >= fNMaxSamples){
    fNMaxSamples += 2;
    fSamples.Set(fNMaxSamples);
  }
  fSamples[fNSamples] = Value;
  fNSamples++;
}
