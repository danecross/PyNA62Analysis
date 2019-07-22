// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
//
// --------------------------------------------------------------
#include "TGigaTrackerHit.hh"

ClassImp(TGigaTrackerHit)

TGigaTrackerHit::TGigaTrackerHit() : TDetectorVHit(), GigaTrackerChannelID() {
}

Int_t TGigaTrackerHit::EncodeChannelID() {
  fChannelID = GigaTrackerChannelID::EncodeChannelID();
  return fChannelID;
}

void TGigaTrackerHit::DecodeChannelID() {
  GigaTrackerChannelID::DecodeChannelID(fChannelID);
}

void TGigaTrackerHit::Clear(Option_t* option){
  TDetectorVHit::Clear(option);
  GigaTrackerChannelID::Clear(option);
}
