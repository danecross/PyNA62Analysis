#include "TSlimRecoCHANTICandidate.hh"
#include "TSlimRecoCHANTIHit.hh"
#include "TSlimRecoCHANTIEvent.hh"

#include "TRecoCHANTIHit.hh"
#include "TRecoCHANTICandidate.hh"
#include "TRecoCHANTIEvent.hh"
#include "TCHANTIDigi.hh"

ClassImp(TSlimRecoCHANTIEvent)

TSlimRecoCHANTIEvent::TSlimRecoCHANTIEvent(TRecoCHANTIEvent* evReco)
{
    FromReco(evReco);
}

void TSlimRecoCHANTIEvent::FromReco(TRecoVEvent* evVReco)
{
    TRecoCHANTIEvent *evReco = static_cast<TRecoCHANTIEvent*>(evVReco);
    Reset();
    fHits.reserve(evReco->GetNHits());
    fCandidates.reserve(evReco->GetNCandidates());
    for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
        auto hitReco = static_cast<TRecoCHANTIHit*>(evReco->GetHit(ihit));
        AddHit(TSlimRecoCHANTIHit(hitReco));
    }

    for (int icand = 0; icand < evReco->GetNCandidates(); icand++) {
        auto candReco = static_cast<TRecoCHANTICandidate*>(evReco->GetCandidate(icand));
        AddCandidate(TSlimRecoCHANTICandidate(candReco));
    }
    SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoCHANTIEvent::ToReco(TRecoVEvent* evVReco)
{
    TRecoCHANTIEvent *evReco = static_cast<TRecoCHANTIEvent*>(evVReco);
    evReco->Clear("C");

    TCHANTIDigi* specDigi = new TCHANTIDigi(); // temporary Digi object
    for (TSlimRecoCHANTIHit &slimHit : fHits) {
        auto recoHit = static_cast<TRecoCHANTIHit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    for (TSlimRecoCHANTICandidate &slimCandidate : fCandidates) {
        auto *recoCandidate = static_cast<TRecoCHANTICandidate*>(evReco->AddCandidate());
        slimCandidate.ToReco(recoCandidate);
    }
    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoCHANTIEvent::Reset()
{
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
    fHits.clear();
}

void TSlimRecoCHANTIEvent::ClearHits() {
    fHits.clear();
}

void TSlimRecoCHANTIEvent::ClearCandidates() {
    fCandidates.clear();
}
