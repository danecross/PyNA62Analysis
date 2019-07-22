// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#include "HACRawDecoder.hh"
#include "HACReconstruction.hh"
#include "EventHeader.hh"
#include "TDCBRawDecoder.hh"

HACRawDecoder::HACRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "HAC") {
  fLibrary.push_back(new TDCBRawDecoder(Reco));
  // fLibrary.push_back(new TELL1RawDecoder(kTRUE,kTRUE));
  // fDecoder = static_cast<NA62VRawDecoder*>(Find("TELL1"));
  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

HACRawDecoder::~HACRawDecoder() {}

void HACRawDecoder::Init() {
  if(fDecoder) fDecoder->Init();
}

void HACRawDecoder::Reset() {
  if(fDecoder) fDecoder->Reset();
}

void HACRawDecoder::StartOfBurst() {
  if(fDecoder) fDecoder->StartOfBurst();
}

void HACRawDecoder::EndOfBurst() {
  if(fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * HACRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset) {
  if (fDecoder) return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else return 0;
}
