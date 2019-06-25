// ---------------------------------------------------------------
// History:
//
// Created by Antonio Cassese (Antonio.Cassese@cern.ch) 2012-10-19
// Updated: E Goudzovski (eg@hep.ph.bham.ac.uk)         2015-11-02
//
// ---------------------------------------------------------------

#ifndef TMUV3Digi_H
#define TMUV3Digi_H

#include "TDCVHit.hh"
#include "MUV3ChannelID.hh"

class TMUV3Digi : public TDCVHit, public MUV3ChannelID {

public:

  TMUV3Digi();
  ~TMUV3Digi() {}

  void Clear(Option_t* = "");

  Int_t  EncodeChannelID();
  void   DecodeChannelID();
  Int_t  GetStationID()    { return 0;                    }
  Bool_t HasLeadingEdge()  { return  GetDetectedEdge()&1; }
  Bool_t HasTrailingEdge() { return  GetDetectedEdge()&2; }

private:

  ClassDef(TMUV3Digi,1);
};

#endif
