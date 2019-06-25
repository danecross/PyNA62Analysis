// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "HACRawEncoder.hh"
#include "HACReconstruction.hh"
#include "TDCBRawEncoder.hh"

HACRawEncoder::HACRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "HAC") 
{
  //fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

HACRawEncoder::~HACRawEncoder() {}

BinaryEvent* HACRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  //printf ("Defining the encoder for the HAC\n");
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void HACRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void HACRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
