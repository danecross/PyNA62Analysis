#include "TSlimRecoLAVHit.hh"
#include "LAVChannelID.hh"
#include "TRecoLAVHit.hh"
#include "iostream"

ClassImp(TSlimRecoLAVHit)

TSlimRecoLAVHit::TSlimRecoLAVHit() :
  TSlimRecoVHit(),
  fChannelID(0),
  fTime(0),
  fEdgeMask(0),
  fLeadingEdgeLow(0),
  fTrailingEdgeLow(0),
  fLeadingEdgeHigh(0),
  fTrailingEdgeHigh(0)
{
}

TSlimRecoLAVHit::TSlimRecoLAVHit(TRecoLAVHit *hitReco):
  fChannelID(0),
  fTime(0),
  fEdgeMask(0),
  fLeadingEdgeLow(0),
  fTrailingEdgeLow(0),
  fLeadingEdgeHigh(0),
  fTrailingEdgeHigh(0)
{
  FromReco(hitReco);
}


void TSlimRecoLAVHit::FromReco(TRecoVHit* hitVReco)
{
  TRecoLAVHit *hitReco = static_cast<TRecoLAVHit*>(hitVReco);
  SetChannelID(hitReco->GetChannelID());
  SetTime(hitReco->GetTime());
  int edgeMask = hitReco->GetEdgeMask();
  if (edgeMask & 1) SetLeadingEdgeLow(hitReco->GetLeadingEdgeLow());
  if (edgeMask & 2) SetLeadingEdgeHigh (hitReco->GetLeadingEdgeHigh());
  if (edgeMask & 4) SetTrailingEdgeHigh(hitReco->GetTrailingEdgeHigh());
  if (edgeMask & 8) SetTrailingEdgeLow (hitReco->GetTrailingEdgeLow());
}

void TSlimRecoLAVHit::ToReco(TRecoVHit* hitVReco)
{
    TRecoLAVHit *hitReco = static_cast<TRecoLAVHit*>(hitVReco);

    // Member variables
    hitReco->SetChannelID(GetChannelID());
    hitReco->DecodeChannelID();
    hitReco->SetTime(GetTime());
    if (fEdgeMask & 1) hitReco->SetLeadingEdgeLow  (GetLeadingEdgeLow());
    if (fEdgeMask & 2) hitReco->SetLeadingEdgeHigh (GetLeadingEdgeHigh());
    if (fEdgeMask & 4) hitReco->SetTrailingEdgeHigh(GetTrailingEdgeHigh());
    if (fEdgeMask & 8) hitReco->SetTrailingEdgeLow (GetTrailingEdgeLow());
}

TVector3 TSlimRecoLAVHit::GetBlockPosition() const {
  TVector3 position;
  TRecoLAVHit::GetBlockPositionFromCh(fChannelID,position);
  return position;
}

Int_t TSlimRecoLAVHit::GetBlockIDFromPhi(Double_t phi, Int_t station, Int_t L) const { // phi in [-pi,pi); station in [1,12], layer in [0,4/5]
  return TRecoLAVHit::GetBlockIDFromPhi(phi,station,L);
}

Double_t TSlimRecoLAVHit::GetBlockPhiSpan(Int_t station) const { // station in [1,12]
  return TRecoLAVHit::GetBlockPhiSpan(station);
}

Int_t TSlimRecoLAVHit::EncodeChannelID(){
  return LAVChannelID::EncodeChannelIDFromInfo(
					       LAVChannelID::GetLAVIDFromCh(fChannelID),
					       LAVChannelID::GetLayerIDFromCh(fChannelID),
					       LAVChannelID::GetBananaIDFromCh(fChannelID),
					       LAVChannelID::GetBlockIDFromCh(fChannelID));
}

Int_t TSlimRecoLAVHit::GetPackedChannelID(){
  /// \MemberDescr
  /// Returns geographical packed channel ID ranging from 0 to nBananas*4*nLayers
  /// \EndMemberDescr
  return LAVChannelID::GetPackedChannelIDFromCh(fChannelID);
}


Int_t TSlimRecoLAVHit::GetLAVID() const { return LAVChannelID::GetLAVIDFromCh(fChannelID);}
Int_t TSlimRecoLAVHit::GetLayerID() const { return LAVChannelID::GetLayerIDFromCh(fChannelID);}
Int_t TSlimRecoLAVHit::GetBananaID() const { return LAVChannelID::GetBananaIDFromCh(fChannelID);}
Int_t TSlimRecoLAVHit::GetBlockID() const { return LAVChannelID::GetBlockIDFromCh(fChannelID);}


//void TSlimRecoLAVHit::Clear(Option_t* option){
//  TRecoVHit::Clear(option);
//  fEdgeMask = 0;
//}
