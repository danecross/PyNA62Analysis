#include "TSlimRecoSACEvent.hh"
#include "TSlimRecoSACHit.hh"
#include "TRecoSACHit.hh"
#include "TRecoSACCandidate.hh"
#include "TRecoSACEvent.hh"
#include "TSACDigi.hh"

ClassImp(TSlimRecoSACEvent)

void TSlimRecoSACEvent::FromReco(TRecoVEvent* evVReco)
{
    TRecoSACEvent *evReco = static_cast<TRecoSACEvent*>(evVReco);
    Reset();
    fCandidates.reserve(evReco->GetNHits());
    for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
        auto hitReco = static_cast<TRecoSACHit*>(evReco->GetHit(ihit));
        AddCandidate(TSlimRecoSACCandidate(hitReco));
    }

    SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoSACEvent::ToReco(TRecoVEvent* evVReco)
{
    TRecoSACEvent *evReco = static_cast<TRecoSACEvent*>(evVReco);
    evReco->Clear("C");

    TSACDigi* specDigi = new TSACDigi(); // temporary Digi object
    for (TSlimRecoSACCandidate &slimHit : fCandidates) {
        auto recoHit = static_cast<TRecoSACHit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoSACEvent::Reset()
{
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
}

void TSlimRecoSACEvent::ClearCandidates() {
    fCandidates.clear();
}
