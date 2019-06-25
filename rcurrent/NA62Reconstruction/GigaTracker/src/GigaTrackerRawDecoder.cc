// ---------------------------------------------------------------
// History:
//
// Modified by Bob Velghe (bob.velghe@cern.ch) 2014-12-08
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#include "GigaTrackerRawDecoder.hh"

GigaTrackerRawDecoder::GigaTrackerRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "GigaTracker") {
  fLibrary.push_back(new GTKRawDecoder(Reco));
  fDecoder = static_cast<NA62VRawDecoder*>(Find("GTK")); // GigaTracKer, Trigger Matched decoder.
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

GigaTrackerRawDecoder::~GigaTrackerRawDecoder() {}

void GigaTrackerRawDecoder::Init() {
  if(fDecoder) fDecoder->Init();
}

void GigaTrackerRawDecoder::Reset() {
  if(fDecoder) fDecoder->Reset();
}

void GigaTrackerRawDecoder::StartOfBurst() {
  if(fDecoder) fDecoder->StartOfBurst();
}

void GigaTrackerRawDecoder::EndOfBurst() {
  if(fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * GigaTrackerRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset) {
  if(fDecoder)
    return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else
    return 0;    
}
