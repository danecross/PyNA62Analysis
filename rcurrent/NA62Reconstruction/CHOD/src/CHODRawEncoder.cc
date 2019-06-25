// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "CHODRawEncoder.hh"
#include "CHODReconstruction.hh"
#include "TDCBRawEncoder.hh"

CHODRawEncoder::CHODRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "CHOD") 
{
  fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

CHODRawEncoder::~CHODRawEncoder() {}

BinaryEvent* CHODRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void CHODRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void CHODRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
