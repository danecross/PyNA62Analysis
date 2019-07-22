#include "TRecoHACHit.hh"

ClassImp(TRecoHACHit)

TRecoHACHit::TRecoHACHit() : TRecoVHit(), HACChannelID() {
  fEdgeMask=0; // Mask for the edges present: bit 0-3 Leading, 4-7-Trailing
  fLeadingEdge0=-999.999, fLeadingEdge1=-999.999, fLeadingEdge2=-999.999, fLeadingEdge3=-999.999;
  fTrailingEdge0=-999.999, fTrailingEdge1=-999.999, fTrailingEdge2=-999.999, fTrailingEdge3=-999.999;
  fChargeModuleSection=-1, fToTsumm=-1; // See HACReconstruction.cc
}

Int_t TRecoHACHit::EncodeChannelID() {
  fChannelID = HACChannelID::EncodeChannelID();
  return fChannelID;
}

void TRecoHACHit::DecodeChannelID() {
  HACChannelID::DecodeChannelID(fChannelID);
}

void TRecoHACHit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
  HACChannelID::Clear(option);
  fEdgeMask=0; // Mask for the edges present: bit 0-3 Leading, 4-7-Trailing
  fLeadingEdge0=-999.999, fLeadingEdge1=-999.999, fLeadingEdge2=-999.999, fLeadingEdge3=-999.999;
  fTrailingEdge0=-999.999, fTrailingEdge1=-999.999, fTrailingEdge2=-999.999, fTrailingEdge3=-999.999;
  fChargeModuleSection=-1, fToTsumm=-1; // See HACReconstruction.cc
}

Double_t TRecoHACHit::GetToT(Int_t ithr){
  if (ithr == 0)
    return (fTrailingEdge0 - fLeadingEdge0);
  if (ithr == 1)
    return (fTrailingEdge1 - fLeadingEdge1);
  if (ithr == 2)
    return (fTrailingEdge2 - fLeadingEdge2);
  if (ithr == 3)
    return (fTrailingEdge3 - fLeadingEdge3);
  return -999.999;
}

Double_t TRecoHACHit::GetLeadingEdge(Int_t ithr) {
  if (ithr == 0)
    return fLeadingEdge0;
  if (ithr == 1)
    return fLeadingEdge1;
  if (ithr == 2)
    return fLeadingEdge2;
  if (ithr == 3)
    return fLeadingEdge3;
  return -999.999;
}

void TRecoHACHit::SetLeadingEdge(Int_t ithr, Double_t fLeadTime) {
  if (ithr == 0)
    SetLeadingEdge0(fLeadTime);
  if (ithr == 1)
    SetLeadingEdge1(fLeadTime);
  if (ithr == 2)
    SetLeadingEdge2(fLeadTime);
  if (ithr == 3)
    SetLeadingEdge3(fLeadTime);
}

Double_t TRecoHACHit::GetTrailingEdge(Int_t ithr) {
  if (ithr == 0) 
    return fTrailingEdge0;
  if (ithr == 1)
    return fTrailingEdge1;
  if (ithr == 2)
    return fTrailingEdge2;
  if (ithr == 3)
    return fTrailingEdge3;
  return -999.999;
}

void TRecoHACHit::SetTrailingEdge(Int_t ithr, Double_t fTrailTime) {
  if (ithr == 0) 
    SetTrailingEdge0(fTrailTime);
  if (ithr == 1) 
    SetTrailingEdge1(fTrailTime);
  if (ithr == 2) 
    SetTrailingEdge2(fTrailTime);
  if (ithr == 3) 
    SetTrailingEdge3(fTrailTime);
}

Int_t TRecoHACHit::GetHighestThreshold() {
  if (IsGrade3())
    return 4;
  else if (IsGrade2())
    return 3;
  else if (IsGrade1())
    return 2;
  else if (IsGrade0())
    return 1;
  return -1;
}

Bool_t TRecoHACHit::IsGrade(Int_t ithr) {
  if (ithr == 3)
    return IsGrade3();
  else if (ithr == 2)
    return IsGrade2();
  else if (ithr == 1)
    return IsGrade1();
  else  if (ithr == 0)
    return IsGrade0();
  return kFALSE;
}
