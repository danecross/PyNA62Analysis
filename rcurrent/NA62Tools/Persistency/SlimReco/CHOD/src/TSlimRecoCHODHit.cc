#include "TSlimRecoCHODHit.hh"
#include "TRecoCHODHit.hh"
#include "CHODChannelID.hh"

ClassImp(TSlimRecoCHODHit)

TSlimRecoCHODHit::TSlimRecoCHODHit(TRecoCHODHit* hitReco)
{
    FromReco(hitReco);
}

void TSlimRecoCHODHit::FromReco(TRecoVHit* hitVReco)
{
    TRecoCHODHit *hitReco = static_cast<TRecoCHODHit*>(hitVReco);
    fChannelID = hitReco->GetChannelID();
    fTime      = hitReco->GetTime();
    fTimeWidth = hitReco->GetTimeWidth();
}

void TSlimRecoCHODHit::ToReco(TRecoVHit* hitVReco)
{
    TRecoCHODHit *hitReco = static_cast<TRecoCHODHit*>(hitVReco);
    hitReco->SetChannelID(fChannelID);
    hitReco->DecodeChannelID();
    hitReco->SetTime(fTime);
    hitReco->SetTimeWidth(fTimeWidth);
}

Int_t TSlimRecoCHODHit::GetPlaneID() const {
  return CHODChannelID::DecodeChannelID_Static(fChannelID).PlaneID;
}

Int_t TSlimRecoCHODHit::GetQuadrantID() const {
  return CHODChannelID::DecodeChannelID_Static(fChannelID).QuadrantID;
}

Int_t TSlimRecoCHODHit::GetCounterID() const {
  return CHODChannelID::DecodeChannelID_Static(fChannelID).CounterID;
}
