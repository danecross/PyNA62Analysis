#include "TSlimRecoCHODCandidate.hh"
#include "TSlimRecoCHODHit.hh"
#include "TSlimRecoCHODEvent.hh"

#include "TRecoCHODHit.hh"
#include "TRecoCHODCandidate.hh"
#include "TRecoCHODEvent.hh"
#include "TCHODDigi.hh"

ClassImp(TSlimRecoCHODEvent)

TSlimRecoCHODEvent::TSlimRecoCHODEvent(TRecoCHODEvent* evReco)
{
    FromReco(evReco);
}

void TSlimRecoCHODEvent::FromReco(TRecoVEvent* evVReco)
{
    TRecoCHODEvent *evReco = static_cast<TRecoCHODEvent*>(evVReco);
    Reset();
    fHits.reserve(evReco->GetNHits());
    fCandidates.reserve(evReco->GetNCandidates());
    for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
        auto hitReco = static_cast<TRecoCHODHit*>(evReco->GetHit(ihit));
        AddHit(TSlimRecoCHODHit(hitReco));
    }

    for (int icand = 0; icand < evReco->GetNCandidates(); icand++) {
        auto candReco = static_cast<TRecoCHODCandidate*>(evReco->GetCandidate(icand));
        AddCandidate(TSlimRecoCHODCandidate(candReco));
    }
    SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoCHODEvent::ToReco(TRecoVEvent* evVReco)
{
    TRecoCHODEvent *evReco = static_cast<TRecoCHODEvent*>(evVReco);
    evReco->Clear("C");

    TCHODDigi* specDigi = new TCHODDigi(); // temporary Digi object
    for (TSlimRecoCHODHit &slimHit : fHits) {
        auto recoHit = static_cast<TRecoCHODHit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    for (TSlimRecoCHODCandidate &slimCandidate : fCandidates) {
        auto *recoCandidate = static_cast<TRecoCHODCandidate*>(evReco->AddCandidate());
        slimCandidate.ToReco(recoCandidate);
    }
    evReco->SetNQuadrants(GetNQuadrants());
    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoCHODEvent::Reset()
{
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
    fHits.clear();
}

void TSlimRecoCHODEvent::ClearHits() {
    fHits.clear();
}

void TSlimRecoCHODEvent::ClearCandidates() {
    fCandidates.clear();
}

Int_t TSlimRecoCHODEvent::GetNQuadrants() const {
    Int_t flagQuadrant[4] = {0,0,0,0};

    // start cycle over reco hits to select good hit pairs
    for(auto &cand : fCandidates){
        Int_t quad = fHits[cand.GetHitsIndexes()[0]].GetQuadrantID();
        flagQuadrant[quad] = 1;
    }

    // write number of quadrants fired
    Int_t nQuadrantsFired = 0;
    for(Int_t i=0; i<4; i++) nQuadrantsFired = nQuadrantsFired + flagQuadrant[i];
    return nQuadrantsFired;
}


