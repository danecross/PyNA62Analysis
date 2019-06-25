// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "CHANTIRawEncoder.hh"
#include "CHANTIReconstruction.hh"
#include "TDCBRawEncoder.hh"

CHANTIRawEncoder::CHANTIRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "CHANTI") 
{
  fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

CHANTIRawEncoder::~CHANTIRawEncoder() {}

BinaryEvent* CHANTIRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void CHANTIRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void CHANTIRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
