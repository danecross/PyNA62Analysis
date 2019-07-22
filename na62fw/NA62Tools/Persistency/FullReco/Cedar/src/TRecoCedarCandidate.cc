// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-18
//
// ---------------------------------------------------------------

#include "TRecoCedarCandidate.hh"
#include "math.h"

ClassImp(TRecoCedarCandidate)

TRecoCedarCandidate::TRecoCedarCandidate() : TRecoVCandidate() {
  TRecoCedarCandidate::Clear();
}

void TRecoCedarCandidate::Clear(Option_t* option) {
  TRecoVCandidate::Clear(option);
  SetTime(0.0);
  fNSectors = 0;
  fNPMTs    = 0;
  fIsSelected = kFALSE;
  fDeltaTimeClosestCandidate = 1.e28;
  fNHitsClosestCandidate = 0;
}

void TRecoCedarCandidate::UpdateTime(Double_t HitTime) {
  SetTime((GetTime()*(GetNHits()-1)+HitTime)/GetNHits());
}

void TRecoCedarCandidate::UpdateTime() {
  SetTime(0.0);
  for (int i=0; i<GetNHits(); i++) {
    SetTime(GetTime()+GetHit(i)->GetTime());
  }
  SetTime(GetTime()/GetNHits());
}
