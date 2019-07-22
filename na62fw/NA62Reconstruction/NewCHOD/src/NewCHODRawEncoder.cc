// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

#include "NewCHODRawEncoder.hh"
#include "NewCHODReconstruction.hh"
#include "TDCBRawEncoder.hh"

NewCHODRawEncoder::NewCHODRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "NewCHOD")  {
  fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if (fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

BinaryEvent* NewCHODRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void NewCHODRawEncoder::Init() {
  if (fEncoder) fEncoder->Init();
}

void NewCHODRawEncoder::End() {
  if (fEncoder) fEncoder->End();
}
