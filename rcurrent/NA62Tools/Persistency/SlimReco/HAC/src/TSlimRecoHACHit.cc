#include "TSlimRecoHACHit.hh"
#include "TRecoHACHit.hh"
#include "HACChannelID.hh"

#include <iostream>

ClassImp(TSlimRecoHACHit)

TSlimRecoHACHit::TSlimRecoHACHit() :
  fChannelID(0), fTime(-999), fChargeModuleSection(-1),
  fLeadingEdge {-999.999,-999.999,-999.999,-999.999},
  fTrailingEdge{-999.999,-999.999,-999.999,-999.999}
{
}

TSlimRecoHACHit::TSlimRecoHACHit(TRecoHACHit* hitReco) {
  FromReco(hitReco);
}

void TSlimRecoHACHit::FromReco(TRecoVHit* hitVReco) {
  TRecoHACHit *hitReco = static_cast<TRecoHACHit*>(hitVReco);
  fChannelID = hitReco->GetChannelID();
  fChargeModuleSection = hitReco->GetChargeModuleSection();
  fTime = hitReco->GetTime();
  for(Short_t ithr = 0; ithr < 4; ++ithr){
    SetLeadingEdge(ithr, hitReco->GetLeadingEdge(ithr));
    SetTrailingEdge(ithr, hitReco->GetTrailingEdge(ithr));
  }
}

void TSlimRecoHACHit::SetLeadingEdge(Short_t iThr, Float_t time) {
    if(iThr>=0 && iThr<4)
        fLeadingEdge[iThr] = time;
    else
        std::cerr << "TSlimRecoHACHit::SetLeadingEdge: iThr=" << iThr << " is invalid" << std::endl;
}

void TSlimRecoHACHit::SetTrailingEdge(Short_t iThr, Float_t time) {
    if(iThr>=0 && iThr<4)
        fTrailingEdge[iThr] = time;
    else
        std::cerr << "TSlimRecoHACHit::SetTrailingEdge: iThr=" << iThr << " is invalid" << std::endl;
}

void TSlimRecoHACHit::ToReco(TRecoVHit* hitVReco) {
  TRecoHACHit *hitReco = static_cast<TRecoHACHit*>(hitVReco);
  hitReco->SetChannelID(fChannelID);
  hitReco->SetTime(fTime);
  for(Short_t ithr = 0; ithr < 4; ++ithr){
    hitReco->SetLeadingEdge(ithr, GetLeadingEdge(ithr));
    hitReco->SetTrailingEdge(ithr, GetTrailingEdge(ithr));
  }
  hitReco->SetToTsumm(GetToTSum());
  hitReco->SetChargeModuleSection(fChargeModuleSection);
  hitReco->SetEdgeMask(GetEdgeMask());
  hitReco->DecodeChannelID();
}

Double_t TSlimRecoHACHit::GetToTSum() const {
  Double_t totSum = 0.0;
  for (Short_t ithr = 0; ithr < 4; ++ithr){
    totSum += (fLeadingEdge[ithr] > -999. && fTrailingEdge[ithr]) ?
      fTrailingEdge[ithr] - fLeadingEdge[ithr] : 0.0;
  }

  return totSum;
}

Int_t TSlimRecoHACHit::GetEdgeMask() const {
  Int_t edgeMask = 0;

  for (Short_t ithr = 0; ithr < 4; ++ithr){
    if (fLeadingEdge[ithr] > -999.){
      edgeMask = (1 << ithr) | edgeMask;
    }
    if (fTrailingEdge[ithr] > -999.){
      edgeMask = (1 << (4 + ithr)) | edgeMask;
    }
  }

  return edgeMask;
}

Int_t   TSlimRecoHACHit::GetSiPMID() const {
    return HACChannelID::DecodeChannelID_Static(fChannelID).fSiPMID;
}
Int_t   TSlimRecoHACHit::GetModuleID() const {
    return HACChannelID::DecodeChannelID_Static(fChannelID).fModuleID;
}
