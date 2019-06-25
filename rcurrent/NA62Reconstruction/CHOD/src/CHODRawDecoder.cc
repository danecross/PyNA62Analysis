#include "CHODRawDecoder.hh"
#include "CHODReconstruction.hh"
#include "TDCBRawDecoder.hh"

CHODRawDecoder::CHODRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "CHOD")
{
  fLibrary.push_back(new TDCBRawDecoder(Reco));
  //fLibrary.push_back(new TELL1RawDecoder(kTRUE,kTRUE));
  //fLibrary.push_back(new CAENRawDecoder(kTRUE,kTRUE));

  //  if(((CHODReconstruction*)fReco)->GetCAEN())
  //    fDecoder = static_cast<NA62VRawDecoder*>(Find("CAEN"));
  //else
  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

CHODRawDecoder::~CHODRawDecoder(){}

void CHODRawDecoder::Init(){
    if(fDecoder) fDecoder->Init();
}

void CHODRawDecoder::Reset(){
    if(fDecoder) fDecoder->Reset();
}

void CHODRawDecoder::StartOfBurst() {
  if (fDecoder) fDecoder->StartOfBurst();
}

void CHODRawDecoder::EndOfBurst() {
  if (fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * CHODRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset)
{
  if(fDecoder)
    //return fDecoder->DecodeNextEvent(CurrentWord, iBurst);
    return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else
    return 0;    
}
