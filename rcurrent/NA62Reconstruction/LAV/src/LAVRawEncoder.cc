// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "LAVRawEncoder.hh"
#include "LAVReconstruction.hh"
#include "TDCBRawEncoder.hh"

LAVRawEncoder::LAVRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "LAV") 
{
  fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

LAVRawEncoder::~LAVRawEncoder() {}

BinaryEvent* LAVRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void LAVRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void LAVRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
