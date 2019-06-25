#include "TSlimRecoSpectrometerHit.hh"
#include "TRecoSpectrometerHit.hh"

ClassImp(TSlimRecoSpectrometerHit)

TSlimRecoSpectrometerHit::TSlimRecoSpectrometerHit(TRecoSpectrometerHit* hitReco)
{
    FromReco(hitReco);
}

void TSlimRecoSpectrometerHit::FromReco(TRecoVHit* hitVReco)
{
    TRecoSpectrometerHit *hitReco = static_cast<TRecoSpectrometerHit*>(hitVReco);
    fChannelID = hitReco->GetChannelID();
    fTime = hitReco->GetTime();
    fTimeWidth = hitReco->GetTimeWidth();
    fWireDistance = hitReco->GetWireDistance();
}

void TSlimRecoSpectrometerHit::ToReco(TRecoVHit* hitVReco)
{
    TRecoSpectrometerHit *hitReco = static_cast<TRecoSpectrometerHit*>(hitVReco);
    // Member variables
    hitReco->SetChannelID(fChannelID);
    hitReco->SetTime(fTime);
    hitReco->SetTimeWidth(fTimeWidth);
    hitReco->SetWireDistance(fWireDistance);
    // Various geometric IDs
    hitReco->SetHalfViewID(this->GetHalfViewID());
    hitReco->SetViewID(this->GetViewID());
    hitReco->SetChamberID(this->GetChamberID());
    hitReco->SetPlaneID(this->GetPlaneID());
    hitReco->SetStrawID(this->GetStrawID());
    // Other variables set to non-default values
    hitReco->SetEdgeStatus(this->GetEdgeStatus());
    hitReco->SetRadius(this->GetRadius());
    hitReco->SetNUsedDigis(this->GetNUsedDigis());
    // remaining variables are left initialized to their default constructor values
}
