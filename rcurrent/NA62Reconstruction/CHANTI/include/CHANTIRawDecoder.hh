#ifndef CHANTIRawDecoder_H
#define CHANTIRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class CHANTIRawDecoder : public NA62VRawDecoder {

  public:

    explicit CHANTIRawDecoder(NA62VReconstruction*);
    ~CHANTIRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};
#endif
