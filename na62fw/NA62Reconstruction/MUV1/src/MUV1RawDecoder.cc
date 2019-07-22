// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2012-11-14
//
// ---------------------------------------------------------------

#include "MUV1RawDecoder.hh"
#include "MUV1Reconstruction.hh"
#include "EventHeader.hh"
#include "CREAMRawDecoder.hh"

MUV1RawDecoder::MUV1RawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "MUV1") {
  fLibrary.push_back(new CREAMRawDecoder(Reco));
  fDecoder = static_cast<NA62VRawDecoder*>(Find("CREAM"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

MUV1RawDecoder::~MUV1RawDecoder() {}

void MUV1RawDecoder::Init() {
  if(fDecoder) fDecoder->Init();
}

void MUV1RawDecoder::Reset() {
  if(fDecoder) fDecoder->Reset();
}

void MUV1RawDecoder::StartOfBurst() {
  if (fDecoder) fDecoder->StartOfBurst();
}

void MUV1RawDecoder::EndOfBurst() {
  if (fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * MUV1RawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset) {
  if (fDecoder) return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else return 0;
}
