#ifndef RICHRawDecoder_H
#define RICHRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class RICHRawDecoder : public NA62VRawDecoder {
  public:

    explicit RICHRawDecoder(NA62VReconstruction*);
    ~RICHRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};
#endif
