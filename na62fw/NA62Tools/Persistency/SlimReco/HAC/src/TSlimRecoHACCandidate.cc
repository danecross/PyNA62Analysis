#include "TSlimRecoHACCandidate.hh"

#include "TRecoHACCandidate.hh"

ClassImp(TSlimRecoHACCandidate)

TSlimRecoHACCandidate::TSlimRecoHACCandidate() :
  fTime(-999.999), fCharge(0), fHitsIndexes(0)
{
}

TSlimRecoHACCandidate::TSlimRecoHACCandidate(TRecoHACCandidate* candReco) {
  FromReco(candReco);
}

void TSlimRecoHACCandidate::FromReco(TRecoVCandidate* candVReco) {
  TRecoHACCandidate *candReco = static_cast<TRecoHACCandidate*> (candVReco);
  fTime   = candReco->GetTime();
  fCharge = candReco->GetCharge();
  for (Int_t iHit = 0; iHit < candReco->GetNHits(); iHit++){
    AddHitIndex(candReco->GetHitsIndexes()[iHit]);
  }
}

void TSlimRecoHACCandidate::ToReco(TRecoVCandidate* candVReco) {
  TRecoHACCandidate *candReco = static_cast<TRecoHACCandidate*> (candVReco);
  candReco->SetTime(fTime);
  candReco->SetCharge(fCharge);
  for (const Short_t hitIndex : GetHitsIndices()){
    candReco->AddHit(hitIndex);
  }
}
