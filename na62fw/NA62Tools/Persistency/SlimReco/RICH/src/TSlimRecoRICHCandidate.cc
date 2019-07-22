#include "TSlimRecoRICHCandidate.hh"
#include "TRecoRICHCandidate.hh"

ClassImp(TSlimRecoRICHCandidate)

TSlimRecoRICHCandidate::TSlimRecoRICHCandidate(TRecoRICHCandidate* candReco){
  FromReco(candReco);
}

/*Int_t TSlimRecoRICHCandidate::Compare (const TObject *obj) const
  {
  if (this == obj) return 0;
  Float_t T1 = static_cast<const TRecoRICHCandidate*>(obj)->GetTime();
  Float_t T2 = GetTime();
  if (T1 > T2) return -1;
  if (T1 < T2) return 1;
  return 0;
  }*/

void TSlimRecoRICHCandidate::FromReco(TRecoVCandidate* candVReco){
  TRecoRICHCandidate *candReco = static_cast<TRecoRICHCandidate*>(candVReco);

  SetRingCenter(candReco->GetRingCenter());
  fRingRadius = candReco->GetRingRadius();
  fRingChi2 = candReco->GetRingChi2();
  fRingTime = candReco->GetRingTime();
  fTime = candReco->GetTime();
  fTimeCandidateIndex = candReco->GetTimeCandidateIndex();
  SetRingCenterSingleRing(candReco->GetRingCenterSingleRing());
  SetRingCenterErrorSingleRing(candReco->GetRingCenterErrorSingleRing());
  fRingRadiusSingleRing = candReco->GetRingRadiusSingleRing();
  fRingRadiusErrorSingleRing = candReco->GetRingRadiusErrorSingleRing();
  fRingChi2SingleRing = candReco->GetRingChi2SingleRing();
  fRingTimeSingleRing = candReco->GetRingTimeSingleRing();
  fNIterationsSingleRing = candReco->GetNIterationsSingleRing();

  fHitIndexesSingleRing.clear();
  for(int i=0; i<candReco->GetNHitsSingleRing(); ++i)
    AddHitIndexSingleRing(candReco->GetHitIndexesSingleRing()[i]);

  fHitIndexes.clear();
  for(int i=0; i<candReco->GetNHits(); ++i)
    AddHitIndex(candReco->GetHitsIndexes()[i]);
}

void TSlimRecoRICHCandidate::ToReco(TRecoVCandidate* candVReco){
  TRecoRICHCandidate *candReco = static_cast<TRecoRICHCandidate*>(candVReco);

  candReco->SetRingCenter(GetRingCenter());
  candReco->SetRingRadius(GetRingRadius());
  candReco->SetRingChi2(GetRingChi2());
  candReco->SetRingTime(GetRingTime());
  candReco->SetTime(GetTime());
  candReco->SetTimeCandidateIndex(GetTimeCandidateIndex());
  candReco->SetRingCenterSingleRing(GetRingCenterSingleRing());
  candReco->SetRingCenterErrorSingleRing(GetRingCenterErrorSingleRing());
  candReco->SetRingRadiusSingleRing(GetRingRadiusSingleRing());
  candReco->SetRingRadiusErrorSingleRing(GetRingRadiusErrorSingleRing());
  candReco->SetRingChi2SingleRing(GetRingChi2SingleRing());
  candReco->SetRingTimeSingleRing(GetRingTimeSingleRing());
  candReco->SetNIterationsSingleRing(GetNIterationsSingleRing());

  for(auto hitIndex : fHitIndexes)
      candReco->AddHit(hitIndex);

  TArrayI arr;
  arr.Set(fHitIndexesSingleRing.size());
  int iHit=0;
  for(auto hitIndex : fHitIndexesSingleRing){
    arr[iHit] = hitIndex;
    ++iHit;
  }
  candReco->SetHitIndexesSingleRing(arr.GetArray(), arr.GetSize());
}
