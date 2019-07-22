#include "LKrRawDecoder.hh"
#include "CPDRawDecoder.hh"
#include "SLMRawDecoder.hh"
#include "CREAMRawDecoder.hh"
#include "LKrReconstruction.hh"
#include "EventHeader.hh"

LKrRawDecoder::LKrRawDecoder(NA62VReconstruction* Reco) : NA62VRawDecoder(Reco, "LKr"){
  //    fLibrary.push_back(new CPDRawDecoder());
  fLibrary.push_back(new CREAMRawDecoder(Reco));

  if(static_cast<LKrReconstruction*>(fReco)->GetHwType()==1) {
    fDecoder = static_cast<NA62VRawDecoder*>(Find("CPD"));
    //cout << "Decoding for LKr is CPDRawDecoder" << std::endl;
  }
  else if(static_cast<LKrReconstruction*>(fReco)->GetHwType()==2) {
    fDecoder = static_cast<NA62VRawDecoder*>(Find("SLM"));
    //cout << "Decoding for LKr is SLMRawDecoder" << std::endl;
  }
  else if(static_cast<LKrReconstruction*>(fReco)->GetHwType()==3) {
    fDecoder = static_cast<NA62VRawDecoder*>(Find("CREAM"));
    //cout << "Decoding for LKr is CREAMRawDecoder" << std::endl;
  }
  else
    std::cout << "Decoding for LKr not yet implemented" << std::endl;
  if(fDecoder) {
    fDigiEvent = fDecoder->GetDigiEvent();
    fSpecialTriggerEvent = fDecoder->GetSpecialTriggerEvent();
  }
}

LKrRawDecoder::~LKrRawDecoder(){}

void LKrRawDecoder::Init(){
  if(fDecoder) fDecoder->Init();
}

void LKrRawDecoder::Reset(){
  if(fDecoder) fDecoder->Reset();
}

void LKrRawDecoder::StartOfBurst(){
  if(fDecoder) fDecoder->StartOfBurst();
}

void LKrRawDecoder::EndOfBurst(){
  if(fDecoder) fDecoder->EndOfBurst();
}

TDetectorVEvent * LKrRawDecoder::DecodeNextEvent(UInt_t * CurrentWord, EventHeader * pEventHeader, UInt_t * NextOffset)
{
  if(fDecoder) {
    //      printf ("Defining the decoder for the LKr\n");
    return fDecoder->DecodeNextEvent(CurrentWord, pEventHeader, NextOffset);
  } else
    return 0;
}
