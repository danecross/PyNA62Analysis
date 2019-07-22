// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TCHANTIHit_H
#define TCHANTIHit_H

#include "TDetectorVHit.hh"
#include "CHANTIChannelID.hh"
class TCHANTIHit : public TDetectorVHit, public CHANTIChannelID {

public:
  
  TCHANTIHit();
  void Clear(Option_t* = "");
  Int_t EncodeChannelID();
  void DecodeChannelID();
  Double_t GetDistanceFromSiPM();
  Int_t GetStationID() { return CHANTIChannelID::GetStationID();}
  Int_t GetPlaneID()   { return CHANTIChannelID::GetPlaneID();}

private:

protected:
  
  ClassDef(TCHANTIHit,1);

};
#endif
