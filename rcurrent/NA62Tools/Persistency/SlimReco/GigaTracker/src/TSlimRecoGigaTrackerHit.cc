#include "TSlimRecoGigaTrackerHit.hh"
#include "TRecoGigaTrackerHit.hh"
#include "GigaTrackerChannelID.hh"

ClassImp(TRecoGigaTrackerHit)

TSlimRecoGigaTrackerHit::TSlimRecoGigaTrackerHit(TRecoGigaTrackerHit* hitReco)
{
    FromReco(hitReco);
}

void TSlimRecoGigaTrackerHit::FromReco(TRecoVHit* hitVReco)
{
  TRecoGigaTrackerHit *hitReco = static_cast<TRecoGigaTrackerHit*>(hitVReco);
  fChannelID = hitReco->GetChannelID();
  fRawTime = hitReco->GetRawTime();
  fTime = hitReco->GetTime();
  fToT = hitReco->GetToT();
  fIsPileUpHit = hitReco->GetIsPileUpHit();
  this->SetPosition(hitReco->GetPosition());
}

void TSlimRecoGigaTrackerHit::ToReco(TRecoVHit* hitVReco)
{
  TRecoGigaTrackerHit *hitReco = static_cast<TRecoGigaTrackerHit*>(hitVReco);
  // Member variables
  hitReco->SetChannelID(fChannelID);
  hitReco->DecodeChannelID();
  hitReco->SetRawTime(fRawTime);
  hitReco->SetTime(fTime);
  hitReco->SetToT(fToT);
  hitReco->SetIsPileUpHit(fIsPileUpHit);
  hitReco->SetPosition(GetPosition());
}

void TSlimRecoGigaTrackerHit::SetPosition(TVector3 position)
{
    fPositionX = position.X();
    fPositionY = position.Y();
}

TVector3 TSlimRecoGigaTrackerHit::GetPosition() const
{
  Float_t PositionZ = GigaTrackerChannelID::GetRawPosition(GetStationNo(), fPositionX, fPositionY).Z();
  return TVector3(fPositionX, fPositionY, PositionZ);
}

Int_t TSlimRecoGigaTrackerHit::GetStationNo() const {
    return GigaTrackerChannelID::DecodeChannelID_Static(fChannelID).fStationNo;
}

Int_t TSlimRecoGigaTrackerHit::GetChipID() const {
    return GigaTrackerChannelID::DecodeChannelID_Static(fChannelID).fChipID;
}

Int_t TSlimRecoGigaTrackerHit::GetChipPixelID() const {
    return GigaTrackerChannelID::DecodeChannelID_Static(fChannelID).fChipPixelID;
}

UInt_t TSlimRecoGigaTrackerHit::GetColumn() const{
    return GigaTrackerChannelID::GetColumn(GetChipID(), GetChipPixelID());
}

UInt_t TSlimRecoGigaTrackerHit::GetRow() const{
    return GigaTrackerChannelID::GetRow(GetChipID(), GetChipPixelID());
}
