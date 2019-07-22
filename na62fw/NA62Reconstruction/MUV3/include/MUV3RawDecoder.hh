#ifndef MUV3RawDecoder_H
#define MUV3RawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class MUV3RawDecoder : public NA62VRawDecoder {
  public:

    explicit MUV3RawDecoder(NA62VReconstruction*);
    ~MUV3RawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};
#endif
