#ifndef CHODRawDecoder_H
#define CHODRawDecoder_H

#include "Rtypes.h"
#include "NA62VRawDecoder.hh"
#include "TDetectorVEvent.hh"
#include "EventHeader.hh"

class CHODRawDecoder : public NA62VRawDecoder {
  public:

    explicit CHODRawDecoder(NA62VReconstruction*);
    ~CHODRawDecoder();
    void Init();
    void Reset();
    void StartOfBurst();
    void EndOfBurst();
    TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);

  private:

};
#endif
