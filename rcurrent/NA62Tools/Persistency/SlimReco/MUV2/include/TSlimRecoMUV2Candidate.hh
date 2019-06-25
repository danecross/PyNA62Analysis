#ifndef TSLIMRECOMUV2CANDIDATE_HH
#define TSLIMRECOMUV2CANDIDATE_HH

#include "TSlimRecoMUVCandidate.hh"


class TSlimRecoMUV2Candidate : public TSlimRecoMUVCandidate {

public:
	TSlimRecoMUV2Candidate (){}
	explicit TSlimRecoMUV2Candidate (TRecoVCandidate *cand);
	virtual ~TSlimRecoMUV2Candidate (){}

	void FromReco(TRecoVCandidate *cand);
	void ToReco(TRecoVCandidate *cand);

    ClassDef(TSlimRecoMUV2Candidate, 1)
};


#endif
