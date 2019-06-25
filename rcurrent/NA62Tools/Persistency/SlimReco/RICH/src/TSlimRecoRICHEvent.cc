#include "TSlimRecoRICHCandidate.hh"
#include "TSlimRecoRICHHit.hh"
#include "TSlimRecoRICHEvent.hh"

#include "TRecoRICHHit.hh"
#include "TRecoRICHCandidate.hh"
#include "TRecoRICHEvent.hh"
#include "TRICHDigi.hh"

ClassImp(TSlimRecoRICHEvent)

TSlimRecoRICHEvent::TSlimRecoRICHEvent(TRecoRICHEvent* evReco) {
    FromReco(evReco);
}

void TSlimRecoRICHEvent::FromReco(TRecoVEvent* evVReco)
{
    TRecoRICHEvent *evReco = static_cast<TRecoRICHEvent*>(evVReco);
    Reset();
    fHits.reserve(evReco->GetNHits());
    for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
        auto hitReco = static_cast<TRecoRICHHit*>(evReco->GetHit(ihit));
        AddHit(TSlimRecoRICHHit(hitReco));
    }

    for (int icand = 0; icand < evReco->GetNRingCandidates(); icand++) {
        auto candReco = static_cast<TRecoRICHCandidate*>(evReco->GetRingCandidate(icand));
        AddRingCandidate(TSlimRecoRICHCandidate(candReco));
    }
    for (int icand = 0; icand < evReco->GetNTimeCandidates(); icand++) {
        auto candReco = static_cast<TRecoRICHCandidate*>(evReco->GetTimeCandidate(icand));
        AddTimeCandidate(TSlimRecoRICHCandidate(candReco));
    }
    fNSCTimeCandidates = evReco->GetNSCTimeCandidates();
    SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoRICHEvent::ToReco(TRecoVEvent* evVReco)
{
    TRecoRICHEvent *evReco = static_cast<TRecoRICHEvent*>(evVReco);
    evReco->Clear("C");

    TRICHDigi* specDigi = new TRICHDigi(); // temporary Digi object
    for (TSlimRecoRICHHit &slimHit : fHits) {
        auto recoHit = static_cast<TRecoRICHHit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    //The order matters. FullReco has a single vector in which the time candidates are at the beginning
    //followed by the ring candidates
    // ---------------------------------------------------
    // |       TimeCand              |     RingCand      |
    // ---------------------------------------------------
    // | SCCand      | PMCand        |
    for (TSlimRecoRICHCandidate &slimCandidate : fTimeCandidates) {
        auto *recoCandidate = static_cast<TRecoRICHCandidate*>(evReco->AddCandidate());
        slimCandidate.ToReco(recoCandidate);
    }
    for (TSlimRecoRICHCandidate &slimCandidate : fRingCandidates) {
        auto *recoCandidate = static_cast<TRecoRICHCandidate*>(evReco->AddCandidate());
        slimCandidate.ToReco(recoCandidate);
    }

    evReco->SetNTimeCandidates(fTimeCandidates.size());
    evReco->SetNRingCandidates(fRingCandidates.size());
    evReco->SetNSCTimeCandidates(fNSCTimeCandidates);
    evReco->SetNPMTimeCandidates(fTimeCandidates.size()-fNSCTimeCandidates);
    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoRICHEvent::Reset()
{
    TSlimRecoVEvent::Reset();
    fTimeCandidates.clear();
    fRingCandidates.clear();
    fHits.clear();
    fNSCTimeCandidates = 0;
}

void TSlimRecoRICHEvent::ClearHits() {
    fHits.clear();
}

void TSlimRecoRICHEvent::ClearCandidates() {
    fTimeCandidates.clear();
    fRingCandidates.clear();
    fNSCTimeCandidates = 0;
}

std::vector<TSlimRecoRICHCandidate> TSlimRecoRICHEvent::GetCandidates() {
    std::vector<TSlimRecoRICHCandidate> v;
    std::copy(fTimeCandidates.begin(), fTimeCandidates.end(), std::back_inserter(v));
    std::copy(fRingCandidates.begin(), fRingCandidates.end(), std::back_inserter(v));
    return v;
}
