#include "TSlimRecoMUV0Event.hh"
#include "TSlimRecoMUV0Hit.hh"
#include "TRecoMUV0Hit.hh"
#include "TRecoMUV0Candidate.hh"
#include "TRecoMUV0Event.hh"
#include "TMUV0Digi.hh"

ClassImp(TSlimRecoMUV0Event)

void TSlimRecoMUV0Event::FromReco(TRecoVEvent* evVReco)
{
    TRecoMUV0Event *evReco = static_cast<TRecoMUV0Event*>(evVReco);
    Reset();
    fCandidates.reserve(evReco->GetNHits());
    for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
        auto hitReco = static_cast<TRecoMUV0Hit*>(evReco->GetHit(ihit));
        AddCandidate(TSlimRecoMUV0Candidate(hitReco));
    }

    SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoMUV0Event::ToReco(TRecoVEvent* evVReco)
{
    TRecoMUV0Event *evReco = static_cast<TRecoMUV0Event*>(evVReco);
    evReco->Clear("C");

    TMUV0Digi* specDigi = new TMUV0Digi(); // temporary Digi object
    for (TSlimRecoMUV0Candidate &slimHit : fCandidates) {
        auto recoHit = static_cast<TRecoMUV0Hit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoMUV0Event::Reset()
{
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
}

void TSlimRecoMUV0Event::ClearCandidates() {
    fCandidates.clear();
}
