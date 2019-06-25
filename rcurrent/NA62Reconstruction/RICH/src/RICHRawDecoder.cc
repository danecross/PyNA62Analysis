#include "RICHRawDecoder.hh"
#include "RICHReconstruction.hh"
#include "TDCBRawDecoder.hh"


RICHRawDecoder::RICHRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "RICH")
{
  fLibrary.push_back(new TDCBRawDecoder(Reco));
  //fLibrary.push_back(new TELL1RawDecoder(kTRUE,kTRUE));
  //fLibrary.push_back(new CAENRawDecoder(kTRUE,kTRUE));

  //  if(((RICHReconstruction*)fReco)->GetCAEN())
  //    fDecoder = static_cast<NA62VRawDecoder*>(Find("CAEN"));
  //else
  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

RICHRawDecoder::~RICHRawDecoder(){}

void RICHRawDecoder::Init(){
    if(fDecoder) fDecoder->Init();
}

void RICHRawDecoder::Reset(){
    if(fDecoder) fDecoder->Reset();
}

void RICHRawDecoder::StartOfBurst() {
  if (fDecoder) fDecoder->StartOfBurst();
}

void RICHRawDecoder::EndOfBurst() {
  if (fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * RICHRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset)
{

  if(fDecoder){
    return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  }else{
    return 0;   
  }
    
}
