// ---------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// ---------------------------------------------------------------

#ifndef SAVRawEncoder_H
#define SAVRawEncoder_H

#include "Rtypes.h"
#include "NA62VRawEncoder.hh"
#include "TDetectorVEvent.hh"
#include "BinaryEvent.hh"

class SAVRawEncoder : public NA62VRawEncoder {

public:

  SAVRawEncoder(NA62VReconstruction*);
  ~SAVRawEncoder();
  BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);
  void Init();
  void End();

private:

};

#endif
