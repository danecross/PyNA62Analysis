#ifndef TSLIMRECOMUV2EVENT_HH
#define TSLIMRECOMUV2EVENT_HH

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoMUV2Hit.hh"
#include "TSlimRecoMUV2Candidate.hh"


class TSlimRecoMUV2Event : public TSlimRecoVEvent {

public:
	TSlimRecoMUV2Event ()          { Reset(); }
	virtual ~TSlimRecoMUV2Event () {}

	void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

	void AddCandidate(TSlimRecoMUV2Candidate c) { fCandidates.emplace_back(std::move(c)); }
	void AddHit(TSlimRecoMUV2Hit h)             { fHits.emplace_back(std::move(h));       }

	Int_t GetNHits()                                     const { return fHits.size();       }
	std::vector<TSlimRecoMUV2Hit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                         { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
	Int_t GetNCandidates()                               const { return fCandidates.size(); }
	std::vector<TSlimRecoMUV2Candidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)            { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

	void FromReco(TRecoVEvent *evReco);
	void ToReco(TRecoVEvent *evReco);
private:
	std::vector<TSlimRecoMUV2Hit> fHits;
	std::vector<TSlimRecoMUV2Candidate> fCandidates;

    ClassDef(TSlimRecoMUV2Event, 1)
};

#endif
