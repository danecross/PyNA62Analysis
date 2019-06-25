//
// Created by B. Velghe (bob.velghe@cern.ch) - Dec. 2014
//

#ifndef _GTKTLRawDecoder_hh_
#define _GTKTLRawDecoder_hh_ 1

#include "NA62VRawDecoder.hh"
#include <iostream>
#include "GigaTrackerTLDataBlock.hh"
#include "TGigaTrackerEvent.hh"
#include "TDCEvent.hh"
#include "TSpecialTriggerEvent.hh"


class GTKTLRawDecoder : public NA62VRawDecoder {
public:
  explicit GTKTLRawDecoder(NA62VReconstruction*);
  ~GTKTLRawDecoder();
  TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);
private:
  GTK::GigaTrackerTLDataBlock* mGTKBlock;
  //  TGigaTrackerEvent* mGTKEvt; 
  //TDCEvent* mGTKEvt; 

};

#endif // _GTKTLRawDecoder_hh_
