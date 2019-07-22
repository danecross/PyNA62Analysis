#include "TSlimRecoMUV2Event.hh"
#include "TRecoMUV2Event.hh"
#include "TRecoMUV2Hit.hh"
#include "TRecoMUV2Candidate.hh"
#include "TMUV2Digi.hh"

ClassImp(TSlimRecoMUV2Event)

void TSlimRecoMUV2Event::FromReco(TRecoVEvent* evVReco)
{
	TRecoMUV2Event *evReco = static_cast<TRecoMUV2Event*>(evVReco);
	Reset();
	fHits.reserve(evReco->GetNHits());
	fCandidates.reserve(evReco->GetNCandidates());
	for (int ihit = 0; ihit < evReco->GetNHits(); ihit++) {
		auto hitReco = static_cast<TRecoMUV2Hit*>(evReco->GetHit(ihit));
		AddHit(TSlimRecoMUV2Hit(hitReco));
	}

	for (int icand = 0; icand < evReco->GetNCandidates(); icand++) {
		auto candReco = static_cast<TRecoMUV2Candidate*>(evReco->GetCandidate(icand));
		AddCandidate(TSlimRecoMUV2Candidate(candReco));
	}
	SetErrorMask(evReco->GetErrorMask());
}

void TSlimRecoMUV2Event::ToReco(TRecoVEvent* evVReco)
{
	TRecoMUV2Event *evReco = static_cast<TRecoMUV2Event*>(evVReco);
	evReco->Clear("C");

	TMUV2Digi* specDigi = new TMUV2Digi(); // temporary Digi object
	for (TSlimRecoMUV2Hit &slimHit : fHits) {
		auto recoHit = static_cast<TRecoMUV2Hit*>(evReco->AddHit(specDigi));
		slimHit.ToReco(recoHit);
	}
	delete specDigi;

	for (TSlimRecoMUV2Candidate &slimCandidate : fCandidates) {
		auto *recoCandidate = static_cast<TRecoMUV2Candidate*>(evReco->AddCandidate());
		slimCandidate.ToReco(recoCandidate);
	}
	evReco->SetErrorMask(GetErrorMask());
}

void TSlimRecoMUV2Event::Reset()
{
    TSlimRecoVEvent::Reset();
	fCandidates.clear();
	fHits.clear();
}

void TSlimRecoMUV2Event::ClearHits() {
    fHits.clear();
}

void TSlimRecoMUV2Event::ClearCandidates() {
    fCandidates.clear();
}

