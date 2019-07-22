// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------

#ifndef NewCHODRawEncoder_H
#define NewCHODRawEncoder_H

#include "Rtypes.h"
#include "NA62VRawEncoder.hh"
#include "TDetectorVEvent.hh"
#include "BinaryEvent.hh"

class NewCHODRawEncoder : public NA62VRawEncoder {

public:

  explicit NewCHODRawEncoder(NA62VReconstruction*);
  ~NewCHODRawEncoder() {}
  BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);
  void Init();
  void End();

private:

};

#endif
