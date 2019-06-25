// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#ifndef RICHRawEncoder_H
#define RICHRawEncoder_H

#include "Rtypes.h"
#include "NA62VRawEncoder.hh"
#include "TDetectorVEvent.hh"
#include "BinaryEvent.hh"

class RICHRawEncoder : public NA62VRawEncoder {

public:

  explicit RICHRawEncoder(NA62VReconstruction*);
  ~RICHRawEncoder();
  BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);
  void Init();
  void End();

private:

};

#endif
