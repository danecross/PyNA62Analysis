// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "GigaTrackerRawEncoder.hh"
#include "GigaTrackerReconstruction.hh"
#include "TDCBRawEncoder.hh"

GigaTrackerRawEncoder::GigaTrackerRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "GigaTracker") 
{
  fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

GigaTrackerRawEncoder::~GigaTrackerRawEncoder() {}

BinaryEvent* GigaTrackerRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void GigaTrackerRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void GigaTrackerRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
