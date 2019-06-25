// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#include "SACRawDecoder.hh"
#include "SACReconstruction.hh"
#include "EventHeader.hh"
#include "TDCBRawDecoder.hh"

SACRawDecoder::SACRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "SAC") {
  fLibrary.push_back(new TDCBRawDecoder(Reco));
  // fLibrary.push_back(new TELL1RawDecoder(kTRUE,kTRUE));
  // fDecoder = static_cast<NA62VRawDecoder*>(Find("TELL1"));
  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

SACRawDecoder::~SACRawDecoder() {}

void SACRawDecoder::Init() {
  if(fDecoder) fDecoder->Init();
}

void SACRawDecoder::Reset() {
  if(fDecoder) fDecoder->Reset();
}

void SACRawDecoder::StartOfBurst() {
  if(fDecoder) fDecoder->StartOfBurst();
}

void SACRawDecoder::EndOfBurst() {
  if(fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * SACRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset) {
  if (fDecoder) return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else return 0;
}
