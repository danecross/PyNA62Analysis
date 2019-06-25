// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#ifndef SpectrometerRawDecoder_H
#define SpectrometerRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class SpectrometerRawDecoder : public NA62VRawDecoder {

  public:

    explicit SpectrometerRawDecoder(NA62VReconstruction*);
    ~SpectrometerRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};

#endif
