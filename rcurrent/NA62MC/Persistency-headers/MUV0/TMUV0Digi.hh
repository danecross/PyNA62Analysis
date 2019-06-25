// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-07-16
// Updated by E Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-05
//
// --------------------------------------------------------------

#ifndef TMUV0Digi_H
#define TMUV0Digi_H

#include "TDCVHit.hh"
#include "MUV0ChannelID.hh"

class TMUV0Digi : public TDCVHit, public MUV0ChannelID {

public:

  TMUV0Digi();
  ~TMUV0Digi() {}

  Int_t  EncodeChannelID();
  void   DecodeChannelID();

  void Clear(Option_t* = "");

  Int_t  GetStationID()     { return 0;                    }
  Bool_t HasLeadingEdge()   { return  GetDetectedEdge()&1; }
  Bool_t HasTrailingEdge()  { return  GetDetectedEdge()&2; }

  Int_t GetThresholdType() const { return fChannelID/10; } //Low Threshold: 0, High Threshold: 1
private:

  ClassDef(TMUV0Digi,1);
};
#endif
