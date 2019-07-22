// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "MUV2RawEncoder.hh"
#include "MUV2Reconstruction.hh"
#include "TDCBRawEncoder.hh"

MUV2RawEncoder::MUV2RawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "MUV2") 
{
  //fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

MUV2RawEncoder::~MUV2RawEncoder() {}

BinaryEvent* MUV2RawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void MUV2RawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void MUV2RawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
