#include "TSlimRecoCedarCandidate.hh"
#include "TRecoCedarCandidate.hh"

ClassImp(TSlimRecoCedarCandidate)

TSlimRecoCedarCandidate::TSlimRecoCedarCandidate() : fNSectors (-1), fTime(-999.9){
}

TSlimRecoCedarCandidate::TSlimRecoCedarCandidate(TRecoCedarCandidate* candReco){
  FromReco(candReco);
}

void TSlimRecoCedarCandidate::FromReco(TRecoVCandidate* candVReco){
  TRecoCedarCandidate *candReco = static_cast<TRecoCedarCandidate*>(candVReco);
  fTime = candReco->GetTime();
  fNSectors = candReco->GetNSectors();
  fHitsIndexes.clear();
  for(int i=0; i<candReco->GetNHits(); ++i) AddHitIndex(candReco->GetHitsIndexes()[i]);
}

void TSlimRecoCedarCandidate::ToReco(TRecoVCandidate* candVReco){
  TRecoCedarCandidate *candReco = static_cast<TRecoCedarCandidate*>(candVReco);

  for(const Short_t hitIndex : fHitsIndexes) candReco->AddHit(hitIndex);
  candReco->SetTime(GetTime());
  candReco->SetNSectors(GetNSectors());
}
