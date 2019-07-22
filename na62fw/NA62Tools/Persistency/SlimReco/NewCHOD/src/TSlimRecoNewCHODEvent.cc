// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#include "TSlimRecoNewCHODEvent.hh"

#include "TSlimRecoNewCHODCandidate.hh"
#include "TRecoNewCHODHit.hh"
#include "TRecoNewCHODEvent.hh"
#include "TNewCHODDigi.hh"

ClassImp(TSlimRecoNewCHODEvent)

TSlimRecoNewCHODEvent::TSlimRecoNewCHODEvent() {}

void TSlimRecoNewCHODEvent::FromReco(TRecoVEvent* evVReco) {
  TRecoNewCHODEvent *evReco = static_cast<TRecoNewCHODEvent*>(evVReco);
  Reset();

  //We transform hits into candidates
  fCandidates.reserve(evReco->GetNHits());
  for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
    auto hitReco = static_cast<TRecoNewCHODHit*>(evReco->GetHit(ihit));
    AddCandidate(TSlimRecoNewCHODCandidate(hitReco));
  }
  SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoNewCHODEvent::ToReco(TRecoVEvent* evVReco) {
  TRecoNewCHODEvent *evReco = static_cast<TRecoNewCHODEvent*>(evVReco);
  evReco->Clear("C");

  //We transform back candidates into Hits
  TNewCHODDigi* Digi = new TNewCHODDigi(); // temporary Digi object
  for (TSlimRecoNewCHODCandidate &slimCand : fCandidates) {
    auto recoHit = static_cast<TRecoNewCHODHit*>(evReco->AddHit(Digi));
    slimCand.ToReco(recoHit);
  }
  delete Digi;
  evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoNewCHODEvent::Reset() {
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
}

void TSlimRecoNewCHODEvent::ClearCandidates() {
    fCandidates.clear();
}
