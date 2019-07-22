// ---------------------------------------------------------------
// History:
// 
// Modified by Bob Velghe (bob.velghe@cern.ch) 2014-12-08
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#ifndef GigaTrackerRawDecoder_H
#define GigaTrackerRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

//#include "GigaTrackerReconstruction.hh"
#include "GTKTLRawDecoder.hh"
#include "GTKRawDecoder.hh"

class GigaTrackerReconstruction;

class GigaTrackerRawDecoder : public NA62VRawDecoder {

  public:

    explicit GigaTrackerRawDecoder(NA62VReconstruction*);
    ~GigaTrackerRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent *  DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};

#endif
