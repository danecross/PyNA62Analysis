//
// Created by B. Velghe (bob.velghe@cern.ch) - Dec. 2014
//

#ifndef _GTKRawDecoder_hh_
#define _GTKRawDecoder_hh_ 1

#include "NA62VRawDecoder.hh"
#include <iostream>
// Note: To be adapted to the actual decoder.
//#include "GtkDataBlock.hh"

class GTKRawDecoder : public NA62VRawDecoder {
    public:
        GTKRawDecoder();
        ~GTKRawDecoder();
        TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*);
    private:

};

#endif // _GTKRawDecoder_hh_
