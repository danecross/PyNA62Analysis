#include "TSlimRecoGigaTrackerCandidate.hh"
#include "TSlimRecoGigaTrackerHit.hh"
#include "TSlimRecoGigaTrackerEvent.hh"

#include "TRecoGigaTrackerHit.hh"
#include "TRecoGigaTrackerCandidate.hh"
#include "TRecoGigaTrackerEvent.hh"
#include "TGigaTrackerDigi.hh"
#include "TRecoGigaTrackerEvent.hh"

ClassImp(TSlimRecoGigaTrackerEvent)

void TSlimRecoGigaTrackerEvent::FromReco(TRecoVEvent* evVReco)
{
    TRecoGigaTrackerEvent *evReco = static_cast<TRecoGigaTrackerEvent*>(evVReco);
    Reset();
    fHits.reserve(evReco->GetNHits());
    fCandidates.reserve(evReco->GetNCandidates());
    for (int ihit = 0; ihit < evReco->GetNHits(); ++ihit) {
        auto hitReco = static_cast<TRecoGigaTrackerHit*>(evReco->GetHit(ihit));
        AddHit(TSlimRecoGigaTrackerHit(hitReco));
    }

    for (int icand = 0; icand < evReco->GetNCandidates(); ++icand) {
        auto candReco = static_cast<TRecoGigaTrackerCandidate*>(evReco->GetCandidate(icand));
        AddCandidate(TSlimRecoGigaTrackerCandidate(candReco));
    }
    SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoGigaTrackerEvent::ToReco(TRecoVEvent* evVReco)
{
    TRecoGigaTrackerEvent *evReco = static_cast<TRecoGigaTrackerEvent*>(evVReco);
    evReco->Clear("C");

    TGigaTrackerDigi* specDigi = new TGigaTrackerDigi(); // temporary Digi object
    for (TSlimRecoGigaTrackerHit &slimHit : fHits) {
        auto recoHit = static_cast<TRecoGigaTrackerHit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    for (TSlimRecoGigaTrackerCandidate &slimCandidate : fCandidates) {
        auto *recoCandidate = static_cast<TRecoGigaTrackerCandidate*>(evReco->AddCandidate());
        slimCandidate.ToReco(recoCandidate);
    }
    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoGigaTrackerEvent::Reset()
{
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
    fHits.clear();
}

void TSlimRecoGigaTrackerEvent::ClearHits() {
    fHits.clear();
}

void TSlimRecoGigaTrackerEvent::ClearCandidates() {
    fCandidates.clear();
}
