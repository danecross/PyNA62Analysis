#ifndef LAVRawDecoder_H
#define LAVRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class LAVRawDecoder : public NA62VRawDecoder {
  public:

    explicit LAVRawDecoder(NA62VReconstruction*);
    ~LAVRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);	

  private:
};

#endif
