#include "TSlimRecoCedarHit.hh"
#include "TRecoCedarHit.hh"
#include "CedarChannelID.hh"

ClassImp(TSlimRecoCedarHit)

TSlimRecoCedarHit::TSlimRecoCedarHit() :
    fChannelID(-1), fWidth(0.0), fTime(-99.9)
{}

TSlimRecoCedarHit::TSlimRecoCedarHit(TRecoCedarHit* hitReco)
{
    FromReco(hitReco);
}

void TSlimRecoCedarHit::FromReco(TRecoVHit* hitVReco)
{
    TRecoCedarHit *hitReco = static_cast<TRecoCedarHit*>(hitVReco);
    SetWidth( hitReco->GetWidth() );
    SetChannelID( hitReco->GetChannelID() );
    SetTime( hitReco->GetTime() );
}

void TSlimRecoCedarHit::ToReco(TRecoVHit* hitVReco)
{
    TRecoCedarHit *hitReco = static_cast<TRecoCedarHit*>(hitVReco);
    hitReco->SetChannelID(GetChannelID());
    hitReco->DecodeChannelID();
    hitReco->SetWidth(GetWidth());
    hitReco->SetTime(GetTime());
    hitReco->SetROChannelID(-1);
}

Int_t TSlimRecoCedarHit::GetSectorID() const {
    return CedarChannelID::DecodeChannelID_Static(fChannelID).SectorID;
}

Int_t TSlimRecoCedarHit::GetRowID() const {
    return CedarChannelID::DecodeChannelID_Static(fChannelID).RowID;
}

Int_t TSlimRecoCedarHit::GetConeID() const {
    return CedarChannelID::DecodeChannelID_Static(fChannelID).ConeID;
}

