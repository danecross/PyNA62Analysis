// --------------------------------------------------------------
// History:
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
//
// --------------------------------------------------------------
#include "TGigaTrackerDigi.hh"

ClassImp(TGigaTrackerDigi)
  
TGigaTrackerDigi::TGigaTrackerDigi() : TDCVHit(), GigaTrackerChannelID() {

   fDelay = 0;
   fSourceId = 0;
   fFrameCounter = 0;

   fPixelAddress = 0;
   fHitArbiterAddress = 0;
   fPileUpAddress = 0;
  
   fLeadingSelector = 0;
   fLeadingCoarse = 0;
   fLeadingFine = 0;
  
   fTotSelector = 0;
   fTotCoarse = 0;
   fTotFine = 0;

   fIsPileUp = false;

}

void TGigaTrackerDigi::Clear(Option_t* option) {
  TDCVHit::Clear(option);
  GigaTrackerChannelID::Clear(option);
  fDelay = 0;
  fSourceId = 0;
  fFrameCounter = 0;

  fPixelAddress = 0;
  fHitArbiterAddress = 0;
  fPileUpAddress = 0;
  
  fLeadingSelector = 0;
  fLeadingCoarse = 0;
  fLeadingFine = 0;
  
  fTotSelector = 0;
  fTotCoarse = 0;
  fTotFine = 0;
}

Int_t TGigaTrackerDigi::EncodeChannelID() {
  fChannelID = GigaTrackerChannelID::EncodeChannelID();
  return fChannelID;
}

void TGigaTrackerDigi::DecodeChannelID() {
  GigaTrackerChannelID::DecodeChannelID(fChannelID);
}
