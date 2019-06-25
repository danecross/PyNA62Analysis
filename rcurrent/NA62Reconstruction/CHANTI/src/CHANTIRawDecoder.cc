#include "CHANTIRawDecoder.hh"
#include "CHANTIReconstruction.hh"
#include "TDCBRawDecoder.hh"

CHANTIRawDecoder::CHANTIRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "CHANTI"){
  fLibrary.push_back(new TDCBRawDecoder(Reco));
  //fLibrary.push_back(new TELL1RawDecoder(kTRUE,kTRUE));
  //fLibrary.push_back(new CAENRawDecoder(kTRUE,kTRUE));

  //  if(((CHANTIReconstruction*)fReco)->GetCAEN())
  //    fDecoder = static_cast<NA62VRawDecoder*>(Find("CAEN"));
  //else
  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

CHANTIRawDecoder::~CHANTIRawDecoder(){}

void CHANTIRawDecoder::Init(){
  if(fDecoder) fDecoder->Init();
}

void CHANTIRawDecoder::Reset(){
  if(fDecoder) fDecoder->Reset();
}

void CHANTIRawDecoder::StartOfBurst() {
  if (fDecoder) fDecoder->StartOfBurst();
}

void CHANTIRawDecoder::EndOfBurst() {
  if (fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * CHANTIRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset){
  if(fDecoder) return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else return 0;    
}
