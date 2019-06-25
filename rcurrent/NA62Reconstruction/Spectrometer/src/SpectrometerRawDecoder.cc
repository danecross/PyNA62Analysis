#include "SpectrometerRawDecoder.hh"
#include "SpectrometerReconstruction.hh"
#include "EventHeader.hh"
#include "SRBRawDecoder.hh"

SpectrometerRawDecoder::SpectrometerRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "Spectrometer")
{
  fLibrary.push_back(new SRBRawDecoder(Reco));
  fDecoder = static_cast<NA62VRawDecoder*>(Find("SRB"));
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

SpectrometerRawDecoder::~SpectrometerRawDecoder() {}

void SpectrometerRawDecoder::Init() {
  //if(fDecoder) fDecoder->Init();
  if(fDecoder) static_cast<SRBRawDecoder*>(fDecoder)->Init(); //needed for the custum Init()
}

void SpectrometerRawDecoder::Reset() {
  if(fDecoder) fDecoder->Reset();
}

void SpectrometerRawDecoder::StartOfBurst() {
  if(fDecoder) fDecoder->StartOfBurst();
}

void SpectrometerRawDecoder::EndOfBurst() {
  if(fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * SpectrometerRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset) {
  if(fDecoder)
    return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  else
    return 0;
}


////#include "SpectrometerRawDecoder.hh"
////#include "TELL1RawDecoder.hh"
////#include "SpectrometerReconstruction.hh"
////#include "EventHeader.hh"
////#include "TDCBRawDecoder.hh"

////SpectrometerRawDecoder::SpectrometerRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "Spectrometer") 
////{
////  fLibrary.push_back(new TDCBRawDecoder("config/Spectrometer.conf"));
////  fDecoder = static_cast<NA62VRawDecoder*>(Find("TDCB"));
////}
////
////SpectrometerRawDecoder::~SpectrometerRawDecoder() {}
////
////void SpectrometerRawDecoder::Reset(UInt_t * pDataBuffer, Int_t NWords) {
////  if(fDecoder) fDecoder->Reset(pDataBuffer, NWords);
////}
////
////TDetectorVEvent * SpectrometerRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset) {
////  if(fDecoder)
////    return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
////  else
////    return 0;    
////}
