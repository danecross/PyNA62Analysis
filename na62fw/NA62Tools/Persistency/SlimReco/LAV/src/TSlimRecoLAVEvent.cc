#include "TSlimRecoLAVCandidate.hh"
#include "TSlimRecoLAVHit.hh"
#include "TSlimRecoLAVEvent.hh"

#include "TRecoLAVHit.hh"
#include "TRecoLAVCandidate.hh"
#include "TRecoLAVEvent.hh"
#include "TLAVDigi.hh"

ClassImp(TSlimRecoLAVEvent)

TSlimRecoLAVEvent::TSlimRecoLAVEvent()
{
}

void TSlimRecoLAVEvent::FromReco(TRecoVEvent* evVReco)
{
    TRecoLAVEvent *evReco = static_cast<TRecoLAVEvent*>(evVReco);
    Reset();
    fHits.reserve(evReco->GetNHits());
    fCandidates.reserve(evReco->GetNCandidates());
    for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
        auto hitReco = static_cast<TRecoLAVHit*>(evReco->GetHit(ihit));
        AddHit(TSlimRecoLAVHit(hitReco));
    }

    for (int icand = 0; icand < evReco->GetNCandidates(); icand++) {
        auto candReco = static_cast<TRecoLAVCandidate*>(evReco->GetCandidate(icand));
        AddCandidate(TSlimRecoLAVCandidate(candReco));
    }
    SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoLAVEvent::ToReco(TRecoVEvent* evVReco)
{
    TRecoLAVEvent *evReco = static_cast<TRecoLAVEvent*>(evVReco);
    evReco->Clear("C");

    TLAVDigi* specDigi = new TLAVDigi(); // temporary Digi object
    for (TSlimRecoLAVHit &slimHit : fHits) {
        auto recoHit = static_cast<TRecoLAVHit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    for (TSlimRecoLAVCandidate &slimCandidate : fCandidates) {
        auto *recoCandidate = static_cast<TRecoLAVCandidate*>(evReco->AddCandidate());
        slimCandidate.ToReco(recoCandidate);
    }
    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoLAVEvent::Reset()
{
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
    fHits.clear();
}

void TSlimRecoLAVEvent::ClearHits() {
    fHits.clear();
}

void TSlimRecoLAVEvent::ClearCandidates() {
    fCandidates.clear();
}
