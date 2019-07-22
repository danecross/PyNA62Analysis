#include "TSlimRecoIRCHit.hh"
#include "TSlimRecoIRCEvent.hh"

#include "TRecoIRCHit.hh"
#include "TRecoIRCCandidate.hh"
#include "TRecoIRCEvent.hh"
#include "TIRCDigi.hh"

ClassImp(TSlimRecoIRCEvent)

void TSlimRecoIRCEvent::FromReco(TRecoVEvent* evVReco)
{
    TRecoIRCEvent *evReco = static_cast<TRecoIRCEvent*>(evVReco);
    Reset();
    fCandidates.reserve(evReco->GetNHits());
    for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
        auto hitReco = static_cast<TRecoIRCHit*>(evReco->GetHit(ihit));
        AddCandidate(TSlimRecoIRCCandidate(hitReco));
    }

    SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoIRCEvent::ToReco(TRecoVEvent* evVReco)
{
    TRecoIRCEvent *evReco = static_cast<TRecoIRCEvent*>(evVReco);
    evReco->Clear("C");

    TIRCDigi* specDigi = new TIRCDigi(); // temporary Digi object
    for (TSlimRecoIRCCandidate &slimHit : fCandidates) {
        auto recoHit = static_cast<TRecoIRCHit*>(evReco->AddHit(specDigi));
        slimHit.ToReco(recoHit);
    }
    delete specDigi;

    evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoIRCEvent::Reset()
{
    TSlimRecoVEvent::Reset();
    fCandidates.clear();
}

void TSlimRecoIRCEvent::ClearCandidates() {
    fCandidates.clear();
}
