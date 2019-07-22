// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
//
// --------------------------------------------------------------
#include "TSACHit.hh"

ClassImp(TSACHit)

TSACHit::TSACHit() : TDetectorVHit(), SACChannelID() {
}

Int_t TSACHit::EncodeChannelID() {
  fChannelID = SACChannelID::EncodeChannelID();
  return fChannelID;
}

void TSACHit::DecodeChannelID() {
  SACChannelID::DecodeChannelID(fChannelID);
}

void TSACHit::Clear(Option_t* option){
  TDetectorVHit::Clear(option);
  SACChannelID::Clear(option);
}
