#include "TSlimRecoCedarCandidate.hh"
#include "TSlimRecoCedarHit.hh"
#include "TSlimRecoCedarEvent.hh"

#include "TRecoCedarHit.hh"
#include "TRecoCedarCandidate.hh"
#include "TRecoCedarEvent.hh"
#include "TCedarDigi.hh"

ClassImp(TSlimRecoCedarEvent)

TSlimRecoCedarEvent::TSlimRecoCedarEvent()
{
}

TSlimRecoCedarEvent::TSlimRecoCedarEvent(TRecoCedarEvent* evReco) {
    FromReco(evReco);
}

void TSlimRecoCedarEvent::FromReco(TRecoVEvent* evVReco)
{
    TRecoCedarEvent *evReco = static_cast<TRecoCedarEvent*>(evVReco);
    Reset();
    fHits.reserve(evReco->GetNHits());
    fCandidates.reserve(evReco->GetNCandidates());
    for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
        auto hitReco = static_cast<TRecoCedarHit*>(evReco->GetHit(ihit));
        AddHit(TSlimRecoCedarHit(hitReco));
    }

    for (int icand = 0; icand < evReco->GetNCandidates(); icand++) {
        auto candReco = static_cast<TRecoCedarCandidate*>(evReco->GetCandidate(icand));
        AddCandidate(TSlimRecoCedarCandidate(candReco));
    }
    SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoCedarEvent::ToReco(TRecoVEvent* evVReco)
{
    TRecoCedarEvent *evReco = static_cast<TRecoCedarEvent*>(evVReco);
    evReco->Clear("C");

    TCedarDigi* specDigi = new TCedarDigi(); // temporary Digi object
    for (TSlimRecoCedarHit &slimHit : fHits) {
        auto recoHit = static_cast<TRecoCedarHit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    for (TSlimRecoCedarCandidate &slimCandidate : fCandidates) {
        auto *recoCandidate = static_cast<TRecoCedarCandidate*>(evReco->AddCandidate());
        slimCandidate.ToReco(recoCandidate);
    }
    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoCedarEvent::Reset()
{
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
    fHits.clear();
}

void TSlimRecoCedarEvent::ClearHits() {
    fHits.clear();
}

void TSlimRecoCedarEvent::ClearCandidates() {
    fCandidates.clear();
}
