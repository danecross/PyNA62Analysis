// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
//
// ---------------------------------------------------------------

#ifndef CedarRawDecoder_H
#define CedarRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class CedarRawDecoder : public NA62VRawDecoder {

  public:

    explicit CedarRawDecoder(NA62VReconstruction*);
    ~CedarRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};

#endif
