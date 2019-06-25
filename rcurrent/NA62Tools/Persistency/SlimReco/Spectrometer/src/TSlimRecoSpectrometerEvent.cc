#include "TSlimRecoSpectrometerCandidate.hh"
#include "TSlimRecoSpectrometerHit.hh"
#include "TSlimRecoSpectrometerEvent.hh"

#include "TRecoSpectrometerHit.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "TRecoSpectrometerEvent.hh"
#include "TSpectrometerDigi.hh"

ClassImp(TSlimRecoSpectrometerEvent)

TSlimRecoSpectrometerEvent::TSlimRecoSpectrometerEvent(TRecoSpectrometerEvent* evReco)
{
    FromReco(evReco);
}

void TSlimRecoSpectrometerEvent::FromReco(TRecoVEvent* evVReco)
{
    TRecoSpectrometerEvent *evReco = static_cast<TRecoSpectrometerEvent*>(evVReco);
    Reset();
    fHits.reserve(evReco->GetNHits());
    fCandidates.reserve(evReco->GetNCandidates());
    for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
        auto hitReco = static_cast<TRecoSpectrometerHit*>(evReco->GetHit(ihit));
        AddHit(TSlimRecoSpectrometerHit(hitReco));
    }

    for (int icand = 0; icand < evReco->GetNCandidates(); icand++) {
        auto candReco = static_cast<TRecoSpectrometerCandidate*>(evReco->GetCandidate(icand));
        AddCandidate(TSlimRecoSpectrometerCandidate(candReco));
    }
    SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoSpectrometerEvent::ToReco(TRecoVEvent* evVReco)
{
    TRecoSpectrometerEvent *evReco = static_cast<TRecoSpectrometerEvent*>(evVReco);
    evReco->Clear("C");

    TSpectrometerDigi* specDigi = new TSpectrometerDigi(); // temporary Digi object
    for (TSlimRecoSpectrometerHit &slimHit : fHits) {
        auto recoHit = static_cast<TRecoSpectrometerHit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    for (TSlimRecoSpectrometerCandidate &slimCandidate : fCandidates) {
        auto *recoCandidate = static_cast<TRecoSpectrometerCandidate*>(evReco->AddCandidate());
        slimCandidate.ToReco(recoCandidate);
    }
    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoSpectrometerEvent::Reset()
{
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
    fHits.clear();
}

void TSlimRecoSpectrometerEvent::ClearHits() {
    fHits.clear();
}

void TSlimRecoSpectrometerEvent::ClearCandidates() {
    fCandidates.clear();
}
