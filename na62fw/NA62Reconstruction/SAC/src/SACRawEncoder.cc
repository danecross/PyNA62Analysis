// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "SACRawEncoder.hh"
#include "SACReconstruction.hh"
#include "TDCBRawEncoder.hh"

SACRawEncoder::SACRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "SAC") 
{
  fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

SACRawEncoder::~SACRawEncoder() {}

BinaryEvent* SACRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  //printf ("Defining the encoder for the SAC\n");
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void SACRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void SACRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
