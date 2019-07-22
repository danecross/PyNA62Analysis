// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "MUV1RawEncoder.hh"
#include "MUV1Reconstruction.hh"
#include "TDCBRawEncoder.hh"

MUV1RawEncoder::MUV1RawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "MUV1") 
{
  //fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

MUV1RawEncoder::~MUV1RawEncoder() {}

BinaryEvent* MUV1RawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void MUV1RawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void MUV1RawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
