#include "TSlimRecoLKrEvent.hh"
#include "TRecoLKrEvent.hh"
#include "TRecoLKrHit.hh"
#include "TRecoLKrCandidate.hh"
#include "TLKrDigi.hh"

ClassImp(TSlimRecoLKrEvent)

void TSlimRecoLKrEvent::FromReco(TRecoVEvent *evVReco){
  TRecoLKrEvent *evReco = static_cast<TRecoLKrEvent*>(evVReco);
  Reset();
  fHits.reserve(evReco->GetNHits());
  fCandidates.reserve(evReco->GetNCandidates());
  for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
    auto hitReco = static_cast<TRecoLKrHit*>(evReco->GetHit(ihit));
    AddHit(TSlimRecoLKrHit(hitReco));
  }
  for (int icand = 0; icand < evReco->GetNCandidates(); icand++) {
    auto candReco = static_cast<TRecoLKrCandidate*>(evReco->GetCandidate(icand));
    AddCandidate(TSlimRecoLKrCandidate(candReco));
  }
  fEnergyTotal = evReco->GetEnergyTotal();
  fRecFlag = evReco->GetRecFlag();
  SetErrorMask(evReco->GetErrorMask());

}

void TSlimRecoLKrEvent::ToReco(TRecoVEvent *evVReco){
  TRecoLKrEvent *evReco = static_cast<TRecoLKrEvent*>(evVReco);
  evReco->Clear("C");
  TLKrDigi* specDigi = new TLKrDigi(); // temporary Digi object
  for (TSlimRecoLKrHit &slimHit : fHits) {
    auto recoHit = static_cast<TRecoLKrHit*>(evReco->AddHit(specDigi));
    slimHit.ToReco(recoHit);
  }
  delete specDigi;
  for (TSlimRecoLKrCandidate &slimCandidate : fCandidates) {
    auto *recoCandidate = static_cast<TRecoLKrCandidate*>(evReco->AddCandidate());
    slimCandidate.ToReco(recoCandidate);
  }
  evReco->SetEnergyTotal(fEnergyTotal);
  evReco->SetRecFlag(fRecFlag);
  evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoLKrEvent::Reset(){
  TSlimRecoVEvent::Reset();
  fEnergyTotal = 0.;
  fRecFlag = 0.;
  fHits.clear();
  fCandidates.clear();
}

void TSlimRecoLKrEvent::ClearHits() {
    fHits.clear();
}

void TSlimRecoLKrEvent::ClearCandidates() {
    fCandidates.clear();
}
