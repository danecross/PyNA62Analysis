// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#ifndef MUV1RawEncoder_H
#define MUV1RawEncoder_H

#include "Rtypes.h"
#include "NA62VRawEncoder.hh"
#include "TDetectorVEvent.hh"
#include "BinaryEvent.hh"

class MUV1RawEncoder : public NA62VRawEncoder {

public:

  explicit MUV1RawEncoder(NA62VReconstruction*);
  ~MUV1RawEncoder();
  BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);
  void Init();
  void End();

private:

};

#endif
