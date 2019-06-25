// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#ifndef IRCRawDecoder_H
#define IRCRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class IRCRawDecoder : public NA62VRawDecoder {

  public:

    explicit IRCRawDecoder(NA62VReconstruction*);
    ~IRCRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent *  DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};

#endif
