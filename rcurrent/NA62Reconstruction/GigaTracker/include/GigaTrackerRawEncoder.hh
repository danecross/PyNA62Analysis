// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#ifndef GigaTrackerRawEncoder_H
#define GigaTrackerRawEncoder_H

#include "Rtypes.h"
#include "NA62VRawEncoder.hh"
#include "TDetectorVEvent.hh"
#include "BinaryEvent.hh"

class GigaTrackerRawEncoder : public NA62VRawEncoder {

public:

  explicit GigaTrackerRawEncoder(NA62VReconstruction*);
  ~GigaTrackerRawEncoder();
  BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);
  void Init();
  void End();

private:

};

#endif
