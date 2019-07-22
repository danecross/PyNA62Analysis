// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "RICHRawEncoder.hh"
#include "RICHReconstruction.hh"
#include "TDCBRawEncoder.hh"

RICHRawEncoder::RICHRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "RICH") 
{
  fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

RICHRawEncoder::~RICHRawEncoder() {}

BinaryEvent* RICHRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void RICHRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void RICHRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
