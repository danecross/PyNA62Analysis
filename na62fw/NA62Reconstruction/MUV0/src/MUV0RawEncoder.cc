// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "MUV0RawEncoder.hh"
#include "MUV0Reconstruction.hh"
#include "TDCBRawEncoder.hh"

MUV0RawEncoder::MUV0RawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "MUV0") 
{
  //fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

MUV0RawEncoder::~MUV0RawEncoder() {}

BinaryEvent* MUV0RawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  //printf ("Defining the encoder for the MUV0\n");
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void MUV0RawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void MUV0RawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
