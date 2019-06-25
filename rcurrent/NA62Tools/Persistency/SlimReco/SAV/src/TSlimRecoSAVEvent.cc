#include "TSlimRecoSAVEvent.hh"

#include "TSlimRecoSAVHit.hh"
#include "TRecoSAVHit.hh"
#include "TRecoSAVCandidate.hh"
#include "TRecoSAVEvent.hh"
#include "TSAVDigi.hh"

ClassImp(TSlimRecoSAVEvent)

void TSlimRecoSAVEvent::FromReco(TRecoVEvent* evVReco)
{
    TRecoSAVEvent *evReco = static_cast<TRecoSAVEvent*>(evVReco);
    Reset();
    fHits.reserve(evReco->GetNHits());
    fCandidates.reserve(evReco->GetNCandidates());
    for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
        auto hitReco = static_cast<TRecoSAVHit*>(evReco->GetHit(ihit));
        AddHit(TSlimRecoSAVHit(hitReco));
    }

    for (int icand = 0; icand < evReco->GetNCandidates(); icand++) {
        auto candReco = static_cast<TRecoSAVCandidate*>(evReco->GetCandidate(icand));
        AddCandidate(TSlimRecoSAVCandidate(candReco));
    }
    SetErrorMask(evReco->GetErrorMask());

}

void TSlimRecoSAVEvent::ToReco(TRecoVEvent* evVReco)
{
    TRecoSAVEvent *evReco = static_cast<TRecoSAVEvent*>(evVReco);
    evReco->Clear("C");

    TSAVDigi* specDigi = new TSAVDigi(); // temporary Digi object
    for (TSlimRecoSAVHit &slimHit : fHits) {
        auto recoHit = static_cast<TRecoSAVHit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    for (TSlimRecoSAVCandidate &slimCandidate : fCandidates) {
        auto *recoCandidate = static_cast<TRecoSAVCandidate*>(evReco->AddCandidate());
        slimCandidate.ToReco(recoCandidate);
    }
    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoSAVEvent::Reset()
{
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
    fHits.clear();
}

void TSlimRecoSAVEvent::ClearHits() {
    fHits.clear();
}

void TSlimRecoSAVEvent::ClearCandidates() {
    fCandidates.clear();
}
