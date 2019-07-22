// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
// Updated: E Goudzovski (eg@hep.ph.bham.ac.uk)     2015-11-02
//
// --------------------------------------------------------------

#include "TMUV3Hit.hh"

ClassImp(TMUV3Hit)

TMUV3Hit::TMUV3Hit() : TDetectorVHit(), MUV3ChannelID() {}

void TMUV3Hit::Clear(Option_t* option){
  TDetectorVHit::Clear(option);
  MUV3ChannelID::Clear(option);
}

Int_t TMUV3Hit::EncodeChannelID() {
  fChannelID = MUV3ChannelID::EncodeChannelID();
  return fChannelID;
}

void TMUV3Hit::DecodeChannelID() {
  MUV3ChannelID::DecodeChannelID(fChannelID);
}
