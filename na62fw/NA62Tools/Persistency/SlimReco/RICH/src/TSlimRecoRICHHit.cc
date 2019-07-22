#include "TSlimRecoRICHHit.hh"
#include "TRecoRICHHit.hh"
#include "RICHChannelID.hh"

ClassImp(TSlimRecoRICHHit)

TSlimRecoRICHHit::TSlimRecoRICHHit(TRecoRICHHit* hitReco)
{
  FromReco(hitReco);
}

void TSlimRecoRICHHit::FromReco(TRecoVHit* hitVReco)
{
  TRecoRICHHit *hitReco = static_cast<TRecoRICHHit*>(hitVReco);

  fTime        = hitReco->GetTime();
  fHitQuality  = hitReco->GetHitQuality();
  fTimeWidth   = hitReco->GetTimeWidth();
  fChannelID   = hitReco->GetChannelID();
  fPtolemy     = hitReco->GetPtolemy();
  SetFitPosition(hitReco->GetFitPosition());
}

void TSlimRecoRICHHit::ToReco(TRecoVHit* hitVReco)
{
  TRecoRICHHit *hitReco = static_cast<TRecoRICHHit*>(hitVReco);
  hitReco->SetTime(GetTime());
  hitReco->SetHitQuality(GetHitQuality());
  hitReco->SetTimeWidth(GetTimeWidth());
  hitReco->SetChannelID(GetChannelID());
  hitReco->SetPtolemy(GetPtolemy());
  hitReco->SetFitPosition(GetFitPosition());
  hitReco->DecodeChannelID();
}

Int_t TSlimRecoRICHHit::GetDiskID() const {
  return RICHChannelID::DecodeChannelID_Static(fChannelID).DiskID;
}

Int_t TSlimRecoRICHHit::GetUpDownDiskID() const {
  return RICHChannelID::DecodeChannelID_Static(fChannelID).UpDownDiskID;
}

Int_t TSlimRecoRICHHit::GetSuperCellID() const {
  return RICHChannelID::DecodeChannelID_Static(fChannelID).SuperCellID;
}

Int_t TSlimRecoRICHHit::GetOrSuperCellID() const {
  return RICHChannelID::DecodeChannelID_Static(fChannelID).OrSuperCellID;
}

Int_t TSlimRecoRICHHit::GetPmtID() const {
  return RICHChannelID::DecodeChannelID_Static(fChannelID).PmtID;
}

/*Int_t TSlimRecoRICHHit::GetChannelSeqID() const {
  if(GetOrSuperCellID()<1){
  return GetSuperCellID()*8+GetPmtID()+GetUpDownDiskID()*61*8+GetDiskID()*61*8*2;
  }else{
  return (61*8*2*2+GetSuperCellID()+GetUpDownDiskID()*61+GetDiskID()*61*2);
  }
  }*/


