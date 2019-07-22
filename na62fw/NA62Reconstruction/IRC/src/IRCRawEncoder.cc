// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "IRCRawEncoder.hh"
#include "IRCReconstruction.hh"
#include "TDCBRawEncoder.hh"

IRCRawEncoder::IRCRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "IRC") 
{
  fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

IRCRawEncoder::~IRCRawEncoder() {}

BinaryEvent* IRCRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  //printf ("Defining the encoder for the IRC\n");
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void IRCRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void IRCRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
