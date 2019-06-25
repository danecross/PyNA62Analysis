#ifndef MUV2RawDecoder_H
#define MUV2RawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class MUV2RawDecoder : public NA62VRawDecoder {
  public:

    explicit MUV2RawDecoder(NA62VReconstruction*);
    ~MUV2RawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};
#endif
