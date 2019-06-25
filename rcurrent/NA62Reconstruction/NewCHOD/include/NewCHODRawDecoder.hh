// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

#ifndef NewCHODRawDecoder_H
#define NewCHODRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class NewCHODRawDecoder : public NA62VRawDecoder {

public:
  explicit NewCHODRawDecoder(NA62VReconstruction*);
  ~NewCHODRawDecoder() {}
  void Init();
  void Reset();
  void StartOfBurst();
  void EndOfBurst();
  TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

private:

};
#endif
