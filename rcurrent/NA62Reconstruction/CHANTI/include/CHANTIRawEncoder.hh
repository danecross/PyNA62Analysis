// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#ifndef CHANTIRawEncoder_H
#define CHANTIRawEncoder_H

#include "Rtypes.h"
#include "NA62VRawEncoder.hh"
#include "TDetectorVEvent.hh"
#include "BinaryEvent.hh"

class CHANTIRawEncoder : public NA62VRawEncoder {

public:

  explicit CHANTIRawEncoder(NA62VReconstruction*);
  ~CHANTIRawEncoder();
  BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);
  void Init();
  void End();

private:

};

#endif
