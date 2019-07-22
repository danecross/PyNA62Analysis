// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#include "TSlimRecoMUV3Candidate.hh"
#include "TSlimRecoMUV3Event.hh"
#include "TRecoMUV3Candidate.hh"
#include "TRecoMUV3Event.hh"

ClassImp(TSlimRecoMUV3Event)

TSlimRecoMUV3Event::TSlimRecoMUV3Event() {}

void TSlimRecoMUV3Event::FromReco(TRecoVEvent* evVReco) {
  TRecoMUV3Event *evReco = static_cast<TRecoMUV3Event*>(evVReco);
  Reset();
  fCandidates.reserve(evReco->GetNCandidates());
  for (Int_t i=0; i<evReco->GetNCandidates(); i++) {
    auto candReco = static_cast<TRecoMUV3Candidate*>(evReco->GetCandidate(i));
    AddCandidate(TSlimRecoMUV3Candidate(candReco));
  }
  SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoMUV3Event::ToReco(TRecoVEvent* evVReco) {
  TRecoMUV3Event *evReco = static_cast<TRecoMUV3Event*>(evVReco);
  evReco->Clear("C");
  for (TSlimRecoMUV3Candidate &slimCandidate : fCandidates) {
    auto *recoCandidate = static_cast<TRecoMUV3Candidate*>(evReco->AddCandidate());
    slimCandidate.ToReco(recoCandidate);
  }
  evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoMUV3Event::Reset() {
    TSlimRecoVEvent::Reset();
  fCandidates.clear();
}

void TSlimRecoMUV3Event::ClearCandidates() {
    fCandidates.clear();
}
