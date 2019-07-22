#ifndef TSLIMRECOMUV1EVENT_HH
#define TSLIMRECOMUV1EVENT_HH

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoMUV1Hit.hh"
#include "TSlimRecoMUV1Candidate.hh"


class TSlimRecoMUV1Event : public TSlimRecoVEvent {

public:
	TSlimRecoMUV1Event ()          { Reset(); }
	virtual ~TSlimRecoMUV1Event () {}

	void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

	void AddCandidate(TSlimRecoMUV1Candidate c) { fCandidates.emplace_back(std::move(c)); }
	void AddHit(TSlimRecoMUV1Hit h)             { fHits.emplace_back(std::move(h));       }

	Int_t GetNHits()                                    const { return fHits.size();       }
	std::vector<TSlimRecoMUV1Hit>& GetHits()                  { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                        { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
	Int_t GetNCandidates()                              const { return fCandidates.size(); }
	std::vector<TSlimRecoMUV1Candidate>& GetCandidates()      { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)           { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

	void FromReco(TRecoVEvent *evReco);
	void ToReco(TRecoVEvent *evReco);
private:
	std::vector<TSlimRecoMUV1Hit> fHits;
	std::vector<TSlimRecoMUV1Candidate> fCandidates;

    ClassDef(TSlimRecoMUV1Event, 1)
};

#endif
