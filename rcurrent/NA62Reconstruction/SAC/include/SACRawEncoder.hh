// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#ifndef SACRawEncoder_H
#define SACRawEncoder_H

#include "Rtypes.h"
#include "NA62VRawEncoder.hh"
#include "TDetectorVEvent.hh"
#include "BinaryEvent.hh"

class SACRawEncoder : public NA62VRawEncoder {

public:

  explicit SACRawEncoder(NA62VReconstruction*);
  ~SACRawEncoder();
  BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);
  void Init();
  void End();

private:

};

#endif
