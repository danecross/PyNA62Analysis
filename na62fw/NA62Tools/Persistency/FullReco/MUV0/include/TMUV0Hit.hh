// --------------------------------------------------------------
// History:
//
// Updated: E Goudzovski (eg@hep.ph.bham.ac.uk)     2016-05-11
//
// --------------------------------------------------------------

#ifndef TMUV0HIT_H
#define TMUV0HIT_H

#include "TDetectorVHit.hh"
#include "MUV0ChannelID.hh"

class TMUV0Hit : public TDetectorVHit, public MUV0ChannelID {

public:

  TMUV0Hit();
  ~TMUV0Hit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  Int_t GetStationID() { return 0; }

protected:
    ClassDef(TMUV0Hit,1);
};
#endif
