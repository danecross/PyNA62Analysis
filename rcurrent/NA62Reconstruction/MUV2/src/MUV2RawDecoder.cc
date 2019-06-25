#include "MUV2RawDecoder.hh"
#include "CREAMRawDecoder.hh"

MUV2RawDecoder::MUV2RawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "MUV2")
{
  fLibrary.push_back(new CREAMRawDecoder(Reco));
  fDecoder = static_cast<NA62VRawDecoder*>(Find("CREAM"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

MUV2RawDecoder::~MUV2RawDecoder(){}

void MUV2RawDecoder::Init(){
  if(fDecoder) fDecoder->Init();
}
  
void MUV2RawDecoder::Reset(){
  if(fDecoder) fDecoder->Reset();
}
  
void MUV2RawDecoder::StartOfBurst() {
  if (fDecoder) fDecoder->StartOfBurst();
}

void MUV2RawDecoder::EndOfBurst() {
  if (fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * MUV2RawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset) {
  if (fDecoder) return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else return 0;
}
