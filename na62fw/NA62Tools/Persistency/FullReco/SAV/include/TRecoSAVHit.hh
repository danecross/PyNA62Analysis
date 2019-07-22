// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// --------------------------------------------------------------
#ifndef TRecoSAVHit_H
#define TRecoSAVHit_H

#include "TRecoVHit.hh"
#include "SAVChannelID.hh"

class TRecoSAVHit : public TRecoVHit , public SAVChannelID {

  public:

    TRecoSAVHit();
    ~TRecoSAVHit(){};

    void Clear(Option_t* = "");

    void DecodeChannelID (Int_t ChannelID);
  
  public:
  
    void SetTime(Double_t time) { fTime = time; }
    Double_t GetTime() { return fTime; }
    
    void SetAmplitude(Double_t amplitude) { fAmplitude = amplitude; }
    Double_t GetAmplitude() { return fAmplitude; }
    
    void SetBaseline(Double_t baseline) { fBaseline = baseline; }
    Double_t GetBaseline() { return fBaseline; }

    void SetEnergy(Double_t energy) { fEnergy = energy; }
    Double_t GetEnergy() { return fEnergy; }

  private:
    Double_t fTime;       ///< Hit time.
    Double_t fAmplitude;  ///< Hit amplitude.
    Double_t fBaseline;   ///< Hit baseline.
    Double_t fEnergy;     ///< Hit energy.
  
    ClassDef(TRecoSAVHit,1);
};
#endif
