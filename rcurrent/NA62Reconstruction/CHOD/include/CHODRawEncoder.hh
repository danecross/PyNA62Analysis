// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#ifndef CHODRawEncoder_H
#define CHODRawEncoder_H

#include "Rtypes.h"
#include "NA62VRawEncoder.hh"
#include "TDetectorVEvent.hh"
#include "BinaryEvent.hh"

class CHODRawEncoder : public NA62VRawEncoder {

public:

  explicit CHODRawEncoder(NA62VReconstruction*);
  ~CHODRawEncoder();
  BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);
  void Init();
  void End();

private:

};

#endif
