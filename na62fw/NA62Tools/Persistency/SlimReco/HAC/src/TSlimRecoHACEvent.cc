#include "TSlimRecoHACEvent.hh"
#include "TSlimRecoHACCandidate.hh"
#include "TSlimRecoHACHit.hh"

#include "TRecoHACEvent.hh"
#include "TRecoHACCandidate.hh"
#include "TRecoHACHit.hh"
#include "THACDigi.hh"

ClassImp(TSlimRecoHACEvent)

TSlimRecoHACEvent::TSlimRecoHACEvent()
{
}

TSlimRecoHACEvent::TSlimRecoHACEvent(TRecoHACEvent* evReco) {
  FromReco(evReco);
}

void TSlimRecoHACEvent::FromReco(TRecoVEvent* evVReco) {
  TRecoHACEvent *evReco = static_cast<TRecoHACEvent*> (evVReco);
  Reset();
  fHits.reserve(evReco->GetNHits());
  fCandidates.reserve(evReco->GetNCandidates());
  for (Int_t iHit = 0; iHit < evReco->GetNHits(); iHit++) {
    auto hitReco = static_cast<TRecoHACHit*> (evReco->GetHit(iHit));
    AddHit(TSlimRecoHACHit(hitReco));
  }

  for (Int_t iCand = 0; iCand < evReco->GetNCandidates(); iCand++) {
    auto candReco = static_cast<TRecoHACCandidate*> (evReco->GetCandidate(iCand));
    AddCandidate(TSlimRecoHACCandidate(candReco));
  }
  SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoHACEvent::ToReco(TRecoVEvent* evVReco) {
  TRecoHACEvent *evReco = static_cast<TRecoHACEvent*> (evVReco);
  evReco->Clear("C");

  THACDigi *hacDigi = new THACDigi(); //temporary Digi
  for (TSlimRecoHACHit &slimHit : fHits) {
    auto recoHit = static_cast<TRecoHACHit*> (evReco->AddHit(hacDigi));
    slimHit.ToReco(recoHit);
  }
  delete hacDigi;

  for (TSlimRecoHACCandidate &slimCand : fCandidates) {
    auto recoCand = static_cast<TRecoHACCandidate*> (evReco->AddCandidate());
    slimCand.ToReco(recoCand);
  }
  evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoHACEvent::Reset() {
  TSlimRecoVEvent::Reset();
  fHits.clear();
  fCandidates.clear();
}

void TSlimRecoHACEvent::ClearHits() {
    fHits.clear();
}

void TSlimRecoHACEvent::ClearCandidates() {
    fCandidates.clear();
}
