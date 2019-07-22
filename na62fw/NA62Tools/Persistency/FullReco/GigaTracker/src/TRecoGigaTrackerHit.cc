// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoGigaTrackerHit.hh"

ClassImp(TRecoGigaTrackerHit)

TRecoGigaTrackerHit::TRecoGigaTrackerHit() :
  TRecoVHit(), GigaTrackerChannelID(),
  fToT(0),
  fRawTime(9999),
  fIsPileUpHit(false)
{
}

TRecoGigaTrackerHit::TRecoGigaTrackerHit(const TRecoGigaTrackerHit & hit) :
  TRecoVHit((TRecoVHit &)hit), GigaTrackerChannelID((GigaTrackerChannelID &) hit),
  fToT(hit.fToT),
  fRawTime(hit.fRawTime),
  fIsPileUpHit(hit.fIsPileUpHit)
{
}

Int_t TRecoGigaTrackerHit::EncodeChannelID() {
  fChannelID = GigaTrackerChannelID::EncodeChannelID();
  return fChannelID;
}

void TRecoGigaTrackerHit::DecodeChannelID() {
  GigaTrackerChannelID::DecodeChannelID(fChannelID);
}

void TRecoGigaTrackerHit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
  GigaTrackerChannelID::Clear(option);
  fToT = 0;
  fRawTime = 9999;
}
