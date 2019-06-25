// --------------------------------------------------------------
// History:
//
// Created by Vito Palladno (vito.palladino@cern.ch) 2011-10-11
//
// Totally Modified by Tommaso.Spadaro@cern.ch and Emanuele Leonardi
//
// --------------------------------------------------------------
#ifndef TLAVDigi_H
#define TLAVDigi_H 1

#include "TDCVHit.hh"
#include "LAVChannelID.hh"

using namespace std;

class TLAVDigi : public TDCVHit, public LAVChannelID {

public:

  TLAVDigi() : TDCVHit(), LAVChannelID(){} 
  explicit TLAVDigi(Int_t iCh) : TDCVHit(iCh), LAVChannelID(){}
  ~TLAVDigi(){}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  Int_t GetStationID() { return GetLAVID(); }

  Int_t GetThresholdType() { return fChannelID/1000000; }
  Int_t GetElectronicChannelID();

  void SetEdges(Int_t, Double_t, Double_t);
   
private:
  ClassDef(TLAVDigi,1);
};
#endif
