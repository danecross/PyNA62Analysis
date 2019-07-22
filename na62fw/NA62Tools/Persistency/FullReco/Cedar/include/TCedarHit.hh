// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef TCedarHit_H
#define TCedarHit_H

#include "TDetectorVHit.hh"
#include "CedarChannelID.hh"

class TCedarHit : public TDetectorVHit, public CedarChannelID {

public:

  TCedarHit();
  ~TCedarHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t GetStationID() { return 0; }

  void  SetPMType (Int_t type) { iPMType = type; }
  Int_t GetPMType()            { return iPMType; }

protected:
  ClassDef(TCedarHit,1);

private:
  Int_t iPMType;

};
#endif
