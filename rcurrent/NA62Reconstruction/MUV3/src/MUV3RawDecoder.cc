#include "MUV3RawDecoder.hh"
#include "MUV3Reconstruction.hh"
#include "TDCBRawDecoder.hh"

MUV3RawDecoder::MUV3RawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "MUV3") {
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

MUV3RawDecoder::~MUV3RawDecoder() {}

void MUV3RawDecoder::Init() {
  if (fDecoder) fDecoder->Init();
}

void MUV3RawDecoder::Reset() {
  if (fDecoder) fDecoder->Reset();
}

void MUV3RawDecoder::StartOfBurst() {
  if (fDecoder) fDecoder->StartOfBurst();
}

void MUV3RawDecoder::EndOfBurst() {
  if (fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent* MUV3RawDecoder::DecodeNextEvent
(UInt_t *CurrentWord, EventHeader *pEventHeader, UInt_t *NextOffset) {
  if (fDecoder) return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else return 0;
}
