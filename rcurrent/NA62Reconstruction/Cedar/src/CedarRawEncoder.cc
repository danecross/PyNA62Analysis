// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "CedarRawEncoder.hh"
#include "CedarReconstruction.hh"
#include "TDCBRawEncoder.hh"

CedarRawEncoder::CedarRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "Cedar") 
{
  fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

CedarRawEncoder::~CedarRawEncoder() {}

BinaryEvent* CedarRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void CedarRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void CedarRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
