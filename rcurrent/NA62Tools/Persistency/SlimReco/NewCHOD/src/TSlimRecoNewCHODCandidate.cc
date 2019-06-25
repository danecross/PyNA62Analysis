// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#include "TSlimRecoNewCHODCandidate.hh"

#include "TRecoNewCHODHit.hh"
#include "NewCHODGeometry.hh"
#include "NewCHODChannelID.hh"

ClassImp(TSlimRecoNewCHODCandidate)

TSlimRecoNewCHODCandidate::TSlimRecoNewCHODCandidate() :
  fType(kUndefinedCandidate), fChannel1(-1), fChannel2(-1),
  fTime1(-999.), fTime2(-999.)
{
}

TSlimRecoNewCHODCandidate::TSlimRecoNewCHODCandidate(TRecoNewCHODHit* hitReco) {
  FromReco(hitReco);
}

void TSlimRecoNewCHODCandidate::FromReco(TRecoVHit* hitVReco) {
  TRecoNewCHODHit *hitReco = static_cast<TRecoNewCHODHit*>(hitVReco);
  fTime1      = hitReco->GetTime1();
  fTime2      = hitReco->GetTime2();
  fChannel1   = hitReco->GetChannel1();
  fChannel2   = hitReco->GetChannel2();
  fType       = hitReco->GetType();
}

void TSlimRecoNewCHODCandidate::ToReco(TRecoVHit* hitVReco) {
  TRecoNewCHODHit *hitReco = static_cast<TRecoNewCHODHit*>(hitVReco);
  hitReco->SetTime1(fTime1);
  hitReco->SetTime2(fTime2);
  hitReco->SetTime(GetTime());
  hitReco->SetTime1NoT0(fTime1); // information not stored
  hitReco->SetTime2NoT0(fTime2); // information not stored
  hitReco->SetChannel1(fChannel1);
  hitReco->SetChannel2(fChannel2);
  hitReco->SetROChannel1(-1); // information not stored
  hitReco->SetROChannel2(-1); // information not stored
  hitReco->SetType(fType);
  hitReco->SetX(NewCHODGeometry::GetInstance()->GetTileCentreX(GetTileID()));
  hitReco->SetY(NewCHODGeometry::GetInstance()->GetTileCentreY(GetTileID()));
  hitReco->SetZ(238131.5); // centre of the G10 plate
  hitReco->SetChannelID(fChannel1);
  hitReco->DecodeChannelID();
}

Double_t TSlimRecoNewCHODCandidate::GetX() {
  return NewCHODGeometry::GetInstance()->GetTileCentreX(GetTileID());
}
Double_t TSlimRecoNewCHODCandidate::GetY() {
  return NewCHODGeometry::GetInstance()->GetTileCentreY(GetTileID());
}
Int_t TSlimRecoNewCHODCandidate::GetTileID() {
  return NewCHODChannelID::DecodeChannelID_Static(fChannel1).fTileID;
}
Int_t TSlimRecoNewCHODCandidate::GetQuadrantID() {
  return NewCHODChannelID::DecodeChannelID_Static(fChannel1).fQuadrantID;
}
Int_t TSlimRecoNewCHODCandidate::GetSeqTileID() {
  return NewCHODChannelID::DecodeChannelID_Static(fChannel1).fSeqTileID;
}
