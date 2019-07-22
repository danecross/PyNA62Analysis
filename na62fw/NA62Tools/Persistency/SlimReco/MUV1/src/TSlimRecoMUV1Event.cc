#include "TSlimRecoMUV1Event.hh"
#include "TRecoMUV1Event.hh"
#include "TRecoMUV1Hit.hh"
#include "TRecoMUV1Candidate.hh"
#include "TMUV1Digi.hh"

ClassImp(TSlimRecoMUV1Event)

void TSlimRecoMUV1Event::FromReco(TRecoVEvent* evVReco)
{
	TRecoMUV1Event *evReco = static_cast<TRecoMUV1Event*>(evVReco);
	Reset();
	fHits.reserve(evReco->GetNHits());
	fCandidates.reserve(evReco->GetNCandidates());
	for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
		auto hitReco = static_cast<TRecoMUV1Hit*>(evReco->GetHit(ihit));
		AddHit(TSlimRecoMUV1Hit(hitReco));
	}

	for (int icand = 0; icand < evReco->GetNCandidates(); icand++) {
		auto candReco = static_cast<TRecoMUV1Candidate*>(evReco->GetCandidate(icand));
		AddCandidate(TSlimRecoMUV1Candidate(candReco));
	}
	SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoMUV1Event::ToReco(TRecoVEvent* evVReco)
{
	TRecoMUV1Event *evReco = static_cast<TRecoMUV1Event*>(evVReco);
	evReco->Clear("C");

	TMUV1Digi* specDigi = new TMUV1Digi(); // temporary Digi object
	for (TSlimRecoMUV1Hit &slimHit : fHits) {
		auto recoHit = static_cast<TRecoMUV1Hit*>(evReco->AddHit(specDigi));
		slimHit.ToReco(recoHit);
	}
	delete specDigi;

	for (TSlimRecoMUV1Candidate &slimCandidate : fCandidates) {
		auto *recoCandidate = static_cast<TRecoMUV1Candidate*>(evReco->AddCandidate());
		slimCandidate.ToReco(recoCandidate);
	}
	evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoMUV1Event::Reset()
{
    TSlimRecoVEvent::Reset();
	fHits.clear();
	fCandidates.clear();
}

void TSlimRecoMUV1Event::ClearHits() {
    fHits.clear();
}

void TSlimRecoMUV1Event::ClearCandidates() {
    fCandidates.clear();
}

