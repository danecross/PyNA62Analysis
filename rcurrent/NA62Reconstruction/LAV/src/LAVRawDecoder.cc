#include "LAVRawDecoder.hh"
#include "LAVReconstruction.hh"
#include "TDCBRawDecoder.hh"
#include "TLAVDigi.hh"
#include "TDCBBufferProto.hh"
#include "TDCBBuffer.hh"

LAVRawDecoder::LAVRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "LAV")
{
  fLibrary.push_back(new TDCBRawDecoder(Reco));
  //fLibrary.push_back(new TELL1RawDecoder(kTRUE,kTRUE));
  //fLibrary.push_back(new CAENRawDecoder(kTRUE,kTRUE));

  //  if(((LAVReconstruction*)fReco)->GetCAEN())
  //    fDecoder = static_cast<NA62VRawDecoder*>(Find("CAEN"));
  //else
  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

LAVRawDecoder::~LAVRawDecoder(){}

void LAVRawDecoder::Init(){
    if(fDecoder) fDecoder->Init();
}

void LAVRawDecoder::Reset(){
    if(fDecoder) fDecoder->Reset();
}

void LAVRawDecoder::StartOfBurst() {
  if(fDecoder) fDecoder->StartOfBurst();
}

void LAVRawDecoder::EndOfBurst() {
  if(fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * LAVRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset)
{
  if(fDecoder)
    //return fDecoder->DecodeNextEvent(CurrentWord, iBurst);
    return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else
    return 0;    
}
