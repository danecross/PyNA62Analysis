#ifndef SAVRawDecoder_H
#define SAVRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class SAVRawDecoder : public NA62VRawDecoder {
  public:

    SAVRawDecoder(NA62VReconstruction*);
    ~SAVRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};
#endif
