// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#include "IRCRawDecoder.hh"
#include "IRCReconstruction.hh"
#include "EventHeader.hh"
#include "TDCBRawDecoder.hh"

IRCRawDecoder::IRCRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "IRC") {
  fLibrary.push_back(new TDCBRawDecoder(Reco));
  // fLibrary.push_back(new TELL1RawDecoder(kTRUE,kTRUE));
  // fDecoder = static_cast<NA62VRawDecoder*>(Find("TELL1"));
  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

IRCRawDecoder::~IRCRawDecoder() {}

void IRCRawDecoder::Init() {
  if(fDecoder) fDecoder->Init();
}

void IRCRawDecoder::Reset() {
  if(fDecoder) fDecoder->Reset();
}

void IRCRawDecoder::StartOfBurst() {
  if(fDecoder) fDecoder->StartOfBurst();
}

void IRCRawDecoder::EndOfBurst() {
  if(fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * IRCRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset) {
  if(fDecoder)
    return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else
    return 0;    
}
