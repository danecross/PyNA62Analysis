// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#include "MUV0RawDecoder.hh"
#include "MUV0Reconstruction.hh"
#include "EventHeader.hh"
#include "TDCBRawDecoder.hh"

MUV0RawDecoder::MUV0RawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "MUV0") {
  fLibrary.push_back(new TDCBRawDecoder(Reco));
  // fLibrary.push_back(new TELL1RawDecoder(kTRUE,kTRUE));
  // fDecoder = static_cast<NA62VRawDecoder*>(Find("TELL1"));
  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

MUV0RawDecoder::~MUV0RawDecoder() {}

void MUV0RawDecoder::Init() {
  if(fDecoder) fDecoder->Init();
}

void MUV0RawDecoder::Reset() {
  if(fDecoder) fDecoder->Reset();
}

void MUV0RawDecoder::StartOfBurst() {
  if (fDecoder) fDecoder->StartOfBurst();
}

void MUV0RawDecoder::EndOfBurst() {
  if (fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * MUV0RawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset) {
  if(fDecoder) return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else return 0;
}
