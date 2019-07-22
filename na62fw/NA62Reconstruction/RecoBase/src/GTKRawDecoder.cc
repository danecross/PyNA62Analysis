//
// Created by B. Velghe (bob.velghe@cern.ch) - Dec. 2014
//

#include "GTKRawDecoder.hh"
// The id "GTK_TL" must match the one in GigaTracker/src/GigaTrackerRawDecoder.cc
GTKRawDecoder::GTKRawDecoder() : NA62VRawDecoder(0, "GTK_TL") {
    std::cout << "!GTK TL Decoder is not yet implemented!" << std::endl;
    // Note: To be adapted to the actual decoder.
    // CERNGTK::GtkDataBlock * dec = new CERNGTK::GtkDataBlock();
}
GTKRawDecoder::~GTKRawDecoder() {}

TDetectorVEvent * GTKRawDecoder::DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*) {
    // Note: To be adapted to the actual decoder.
    return 0;
}
