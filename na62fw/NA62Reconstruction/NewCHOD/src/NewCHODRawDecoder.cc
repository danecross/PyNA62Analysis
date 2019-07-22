// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

#include "NewCHODRawDecoder.hh"
#include "NewCHODReconstruction.hh"
#include "TDCBRawDecoder.hh"

NewCHODRawDecoder::NewCHODRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "NewCHOD") {
  fLibrary.push_back(new TDCBRawDecoder(Reco));
  //fLibrary.push_back(new TELL1RawDecoder(kTRUE,kTRUE));
  //fLibrary.push_back(new CAENRawDecoder(kTRUE,kTRUE));

  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
  //fDecoder = static_cast<NA62VRawDecoder*>(Find("CAEN"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

void NewCHODRawDecoder::Init() {
  if (fDecoder) fDecoder->Init();
}

void NewCHODRawDecoder::Reset() {
  if (fDecoder) fDecoder->Reset();
}

void NewCHODRawDecoder::StartOfBurst() {
  if (fDecoder) fDecoder->StartOfBurst();
}

void NewCHODRawDecoder::EndOfBurst() {
  if (fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent* NewCHODRawDecoder::DecodeNextEvent
(UInt_t *CurrentWord, EventHeader *pEventHeader, UInt_t *NextOffset) {
  if (fDecoder) return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else return 0;
}
