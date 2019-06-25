// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Updated: E Goudzovski (eg@hep.ph.bham.ac.uk)     2015-11-02
//
// --------------------------------------------------------------

#ifndef TMUV3Hit_H
#define TMUV3Hit_H

#include "TDetectorVHit.hh"
#include "MUV3ChannelID.hh"

class TMUV3Hit : public TDetectorVHit, public MUV3ChannelID {

public:

  TMUV3Hit();
  ~TMUV3Hit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t  GetStationID()         { return 0;          }
  Bool_t IsMuonHit()            { return fIsMuonHit; }
  void   SetMuonHit(Bool_t val) { fIsMuonHit = val;  }

protected:
  ClassDef(TMUV3Hit,1);

private:
  Bool_t fIsMuonHit;

};
#endif
