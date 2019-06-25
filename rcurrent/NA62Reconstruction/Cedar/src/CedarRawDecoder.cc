// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
//
// ---------------------------------------------------------------

#include "CedarRawDecoder.hh"
#include "CedarReconstruction.hh"
#include "EventHeader.hh"
#include "TDCBRawDecoder.hh"

CedarRawDecoder::CedarRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "Cedar") {
  fLibrary.push_back(new TDCBRawDecoder(Reco));
  // fLibrary.push_back(new TELL1RawDecoder(kTRUE,kTRUE));
  // fDecoder = static_cast<NA62VRawDecoder*>(Find("TELL1"));
  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

CedarRawDecoder::~CedarRawDecoder() {}

void CedarRawDecoder::Init() {
  if (fDecoder) fDecoder->Init();
}

void CedarRawDecoder::Reset() {
  if (fDecoder) fDecoder->Reset();
}

void CedarRawDecoder::StartOfBurst() {
  if (fDecoder) fDecoder->StartOfBurst();
}

void CedarRawDecoder::EndOfBurst() {
  if (fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent* CedarRawDecoder::DecodeNextEvent(UInt_t *CurrentWord, EventHeader *pEventHeader, UInt_t *NextOffset) {
  if (fDecoder) return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else return 0;    
}
