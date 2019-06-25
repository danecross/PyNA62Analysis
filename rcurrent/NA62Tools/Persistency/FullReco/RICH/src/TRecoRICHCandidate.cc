// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoRICHCandidate.hh"

ClassImp(TRecoRICHCandidate)

TRecoRICHCandidate::TRecoRICHCandidate() : TRecoVCandidate(){
  fIsSelected = kFALSE;
  fDeltaTimeClosestCandidate = -999999.;
  fNHitsClosestCandidate = 0;
  fRingCenter.Set(500.,500.);
  fRingRadius = -10;
  fRingChi2 = -10;
  fRingTime = 999999999;
  fTimeCandidateIndex = -1;

  // variables for the iteration fit
  fRingCenterSingleRing.Set(500.,500.);
  fRingCenterErrorSingleRing.Set(0.,0.);
  fRingRadiusSingleRing = -9999.;
  fRingRadiusErrorSingleRing = -9999.;
  fRingChi2SingleRing = -9999.;
  fRingTimeSingleRing = -9999.;
  fNHitsSingleRing = -1;
  fNIterationsSingleRing = -1;
  fHitIndexesSingleRing.Set(0);
}

void TRecoRICHCandidate::Clear(Option_t* /*option*/){
  TRecoVCandidate::Clear();
  SetTime(0.0);
  fIsSelected = kFALSE;
  fDeltaTimeClosestCandidate = -999999.;
  fNHitsClosestCandidate = 0;
  fRingCenter.Set(500.,500.);
  fRingRadius = -10;
  fRingChi2 = -10;
  fRingTime = 999999999;
  fTimeCandidateIndex = -1;

  // variables for the iteration fit
  fRingCenterSingleRing.Set(500.,500.);
  fRingCenterErrorSingleRing.Set(0.,0.);
  fRingRadiusSingleRing = -9999.;
  fRingRadiusErrorSingleRing = -9999.;
  fRingChi2SingleRing = -9999.;
  fRingTimeSingleRing = -9999.;
  fNHitsSingleRing = -1;
  fNIterationsSingleRing = -1;
  fHitIndexesSingleRing.Set(0);
}

void TRecoRICHCandidate::UpdateTime(Double_t HitTime){
  SetTime((GetTime()*(GetNHits()-1)+HitTime)/GetNHits());
}

void TRecoRICHCandidate::UpdateTime(){
  SetTime(0.0);
  for (int i=0; i<GetNHits(); i++) {
    SetTime(GetTime()+GetHit(i)->GetTime());
  }
  SetTime(GetTime()/GetNHits());
}

Int_t TRecoRICHCandidate::Compare (const TObject *obj) const
{
  if (this == obj) return 0;
  Double_t T1 = static_cast<const TRecoRICHCandidate*>(obj)->GetTime();
  Double_t T2 = GetTime();
  if (T1 > T2) return -1;
  if (T1 < T2) return 1;
  return 0;
}
