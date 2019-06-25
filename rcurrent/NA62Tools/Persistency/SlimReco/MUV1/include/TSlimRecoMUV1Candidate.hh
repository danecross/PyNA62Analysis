#ifndef TSLIMRECOMUV1CANDIDATE_HH
#define TSLIMRECOMUV1CANDIDATE_HH

#include "TSlimRecoMUVCandidate.hh"


class TSlimRecoMUV1Candidate : public TSlimRecoMUVCandidate {

public:
	TSlimRecoMUV1Candidate (){}
	explicit TSlimRecoMUV1Candidate (TRecoVCandidate *cand);
	virtual ~TSlimRecoMUV1Candidate (){}

	void FromReco(TRecoVCandidate *cand);
	void ToReco(TRecoVCandidate *cand);

    ClassDef(TSlimRecoMUV1Candidate, 1)
};


#endif
