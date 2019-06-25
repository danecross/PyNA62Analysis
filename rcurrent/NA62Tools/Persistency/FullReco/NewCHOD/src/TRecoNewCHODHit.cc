// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------
/// \class TRecoNewCHODHit
/// \Brief
/// NewCHOD RecoHit: NewCHOD response information (TRecoNewCHODCandidate is empty)
/// \EndBrief

#include "TRecoNewCHODHit.hh"

ClassImp(TRecoNewCHODHit)

TRecoNewCHODHit::TRecoNewCHODHit() : TRecoVHit(), NewCHODChannelID() {
  Clear();
}

Int_t TRecoNewCHODHit::EncodeChannelID() {
  fChannelID = NewCHODChannelID::EncodeChannelID();
  return fChannelID;
}

void TRecoNewCHODHit::DecodeChannelID() {
  NewCHODChannelID::DecodeChannelID(fChannelID);
}

void TRecoNewCHODHit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
  NewCHODChannelID::Clear(option);
  fType = kUndefinedCandidate;
  SetTime(-999.);
  fChannel1 = fChannel2 = fROChannel1 = fROChannel2 = -1;
  fX = fY = 0.0;
  fZ = 238131.5; // centre of the G10 plate
  fTime1 = fTime2 = fTime1NoT0 = fTime2NoT0 = fTimeNoT0 = -999.;
}

// The earliest of the two channel times: recommended to use as track time when |DeltaTime|>5ns
Double_t TRecoNewCHODHit::GetEarliestTime() {
  if (fType==kTightCandidate) return (fTime1 < fTime2) ? fTime1 : fTime2;
  return fTime1;
}

// Time differences (defined for tight candidates only): high-low PMT
Double_t TRecoNewCHODHit::GetDeltaTime() {
  return (fType==kTightCandidate) ? fTime2-fTime1 : 0.0;
}
