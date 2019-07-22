#ifndef LKrRawDecoder_H
#define LKrRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class LKrRawDecoder : public NA62VRawDecoder {
  public:

    explicit LKrRawDecoder(NA62VReconstruction*);
    ~LKrRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};
#endif
