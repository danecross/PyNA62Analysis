// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "MUV3RawEncoder.hh"
#include "MUV3Reconstruction.hh"
#include "TDCBRawEncoder.hh"

MUV3RawEncoder::MUV3RawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "MUV3") 
{
  fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

MUV3RawEncoder::~MUV3RawEncoder() {}

BinaryEvent* MUV3RawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void MUV3RawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void MUV3RawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
