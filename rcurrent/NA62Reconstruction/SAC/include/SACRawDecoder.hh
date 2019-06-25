// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#ifndef SACRawDecoder_H
#define SACRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class SACRawDecoder : public NA62VRawDecoder {

  public:

    explicit SACRawDecoder(NA62VReconstruction*);
    ~SACRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent *  DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};

#endif
