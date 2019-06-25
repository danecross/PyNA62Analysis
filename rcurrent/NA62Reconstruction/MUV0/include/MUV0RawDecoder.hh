// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#ifndef MUV0RawDecoder_H
#define MUV0RawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class MUV0RawDecoder : public NA62VRawDecoder {

  public:

    explicit MUV0RawDecoder(NA62VReconstruction*);
    ~MUV0RawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};

#endif
