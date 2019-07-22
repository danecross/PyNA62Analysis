// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
// Modified by Maria Brigida Brunetti (maria.brigida.brunetti@cern.ch) 2016-01-21
// ---------------------------------------------------------------

#include "SpectrometerRawEncoder.hh"
#include "SpectrometerReconstruction.hh"
#include "SRBRawEncoder.hh"

SpectrometerRawEncoder::SpectrometerRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "Spectrometer") 
{
  fLibrary.push_back(new SRBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("SRB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

SpectrometerRawEncoder::~SpectrometerRawEncoder() {}

BinaryEvent* SpectrometerRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void SpectrometerRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void SpectrometerRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
