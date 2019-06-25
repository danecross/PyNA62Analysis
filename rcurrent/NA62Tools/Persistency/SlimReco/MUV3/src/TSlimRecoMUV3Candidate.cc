// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#include "TSlimRecoMUV3Candidate.hh"
#include "TRecoMUV3Candidate.hh"
#include "MUV3Geometry.hh"

ClassImp(TSlimRecoMUV3Candidate)

TSlimRecoMUV3Candidate::TSlimRecoMUV3Candidate() :
  fType(kUndefinedCandidate), fChannel1(-1), fChannel2(-1),
  fTime(-999.), fTime1(-999.), fTime2(-999.) {}

TSlimRecoMUV3Candidate::TSlimRecoMUV3Candidate(TRecoMUV3Candidate* candReco) {
  FromReco(candReco);
}

void TSlimRecoMUV3Candidate::FromReco(TRecoVCandidate* candVReco) {
  TRecoMUV3Candidate *candReco = static_cast<TRecoMUV3Candidate*>(candVReco);
  fTime     = candReco->GetTime();
  fTime1    = candReco->GetTime1();
  fTime2    = candReco->GetTime2();
  fChannel1 = candReco->GetChannel1();
  fChannel2 = candReco->GetChannel2();
  fType     = candReco->GetType();
}

void TSlimRecoMUV3Candidate::ToReco(TRecoVCandidate* candVReco) {
  TRecoMUV3Candidate *candReco = static_cast<TRecoMUV3Candidate*>(candVReco);

  candReco->SetTime(fTime);
  candReco->SetTimeNoTileT0(fTime); // information not stored
  candReco->SetTimeNoT0(fTime);     // information not stored
  candReco->SetTime1(fTime1);
  candReco->SetTime2(fTime2);
  candReco->SetTime1NoT0(fTime1); // information not stored
  candReco->SetTime2NoT0(fTime2); // information not stored
  candReco->SetChannel1(fChannel1);
  candReco->SetChannel2(fChannel2);
  candReco->SetROChannel1(-1); // information not stored
  candReco->SetROChannel2(-1); // information not stored
  candReco->SetType(fType);

  Int_t TileID = fChannel1%200;
  candReco->SetTileID(TileID);
  candReco->SetX(MUV3Geometry::GetInstance()->GetTileCentreX(TileID));
  candReco->SetY(MUV3Geometry::GetInstance()->GetTileCentreY(TileID));
  candReco->SetZ(246800.0);
}

// Average hit time, as opposed to candidate time, which is the latest hit time
Double_t TSlimRecoMUV3Candidate::GetAverageTime() {
  return (fType==kTightCandidate) ? 0.5*(fTime1+fTime2) : fTime1;
}

// Time difference (defined for tight candidates only): high-low PMT
Double_t TSlimRecoMUV3Candidate::GetDeltaTime() {
  return (fType==kTightCandidate) ? fTime2-fTime1 : 0.0;
}

Double_t TSlimRecoMUV3Candidate::GetX() {
  return MUV3Geometry::GetInstance()->GetTileCentreX(fChannel1%200);
}
Double_t TSlimRecoMUV3Candidate::GetY() {
  return MUV3Geometry::GetInstance()->GetTileCentreY(fChannel1%200);
}
TVector3 TSlimRecoMUV3Candidate::GetPosition() {
  return TVector3(GetX(), GetY(), 246800.0);
}
