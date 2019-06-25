// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "LKrRawEncoder.hh"
#include "LKrReconstruction.hh"
#include "TDCBRawEncoder.hh"

LKrRawEncoder::LKrRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "LKr") 
{
  //fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

LKrRawEncoder::~LKrRawEncoder() {}

BinaryEvent* LKrRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  //printf ("Defining the encoder for the LKr\n");
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void LKrRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void LKrRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
