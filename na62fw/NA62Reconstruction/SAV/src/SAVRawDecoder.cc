#include "SAVRawDecoder.hh"
#include "CREAMRawDecoder.hh"

SAVRawDecoder::SAVRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "SAV") {
  fLibrary.push_back(new CREAMRawDecoder(Reco));
  fDecoder = static_cast<NA62VRawDecoder*>(Find("CREAM"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

SAVRawDecoder::~SAVRawDecoder(){}

void SAVRawDecoder::Init(){
  if(fDecoder) fDecoder->Init();
}

void SAVRawDecoder::Reset(){
  if(fDecoder) fDecoder->Reset();
}

void SAVRawDecoder::StartOfBurst() {
  if(fDecoder) fDecoder->StartOfBurst();
}

void SAVRawDecoder::EndOfBurst() {
  if(fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * SAVRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset) {
  if (fDecoder) return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else return 0;
}
