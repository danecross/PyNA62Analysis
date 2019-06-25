#include "TSlimRecoLKrHit.hh"
#include "TRecoLKrHit.hh"
#include "LKrChannelID.hh"

ClassImp(TSlimRecoLKrHit)

TSlimRecoLKrHit::TSlimRecoLKrHit(TRecoLKrHit* hitReco){
  FromReco(hitReco);
}

Int_t TSlimRecoLKrHit::GetXCellID() const{
  return LKrChannelID::DecodeChannelID_Static(fChannelID).fXCellID;
}

Int_t TSlimRecoLKrHit::GetYCellID() const{
  return LKrChannelID::DecodeChannelID_Static(fChannelID).fYCellID;
}

Int_t TSlimRecoLKrHit::GetCPDID() const {
  return LKrChannelID::DecodeChannelID_Static(fChannelID).fCPDID;
}

Int_t TSlimRecoLKrHit::GetCPDChannelID() const {
  return LKrChannelID::DecodeChannelID_Static(fChannelID).fCPDChannelID;
}

void TSlimRecoLKrHit::FromReco(TRecoVHit* hitVReco)
{
  TRecoLKrHit *hitReco = static_cast<TRecoLKrHit*>(hitVReco);
  fChannelID = hitReco->GetChannelID();
  fTime = hitReco->GetTime();
  fEnergy = hitReco->GetEnergy();
  fPedestal = hitReco->GetPedestal();
}

void TSlimRecoLKrHit::ToReco(TRecoVHit* hitVReco)
{
  TRecoLKrHit *hitReco = static_cast<TRecoLKrHit*>(hitVReco);
  // Member variables
  hitReco->SetChannelID(fChannelID);
  hitReco->DecodeChannelID();
  hitReco->SetTime(fTime);
  hitReco->SetEnergy(fEnergy);
  hitReco->SetPedestal(fPedestal);
  // Various geometric IDs
  hitReco->SetPosition(this->GetPosition());
  // useless variables from Standard Persistency
}
