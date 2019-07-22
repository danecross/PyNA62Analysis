#include "TSlimRecoVEvent.hh"
#include "TRecoVEvent.hh"

ClassImp(TSlimRecoVEvent)

void TSlimRecoVEvent::Reset() {
    fErrorMask = 0;
}

void TSlimRecoVEvent::ClearHits() {
}

void TSlimRecoVEvent::ClearCandidates() {
}
