#ifndef TSLIMRECOMUV1HIT_HH
#define TSLIMRECOMUV1HIT_HH
#include "TSlimRecoMUVHit.hh"
#include <TVector3.h>

class TSlimRecoMUV1Hit : public TSlimRecoMUVHit {

public:
	TSlimRecoMUV1Hit ()               {}
	explicit TSlimRecoMUV1Hit (TRecoVHit *hit);
	virtual ~TSlimRecoMUV1Hit ()      {}

	Int_t GetQuadrant ()              const;
	Float_t GetScintillatorPosition() const;
	Float_t GetEnergy ()              const { return GetCharge()/1.575; }
	Bool_t IsLongScintillator ()      const;
    Int_t GetScintillatorNumber ()    const;
    Int_t GetSide ()                  const;
    TVector3 GetPosition()            const;

	void FromReco(TRecoVHit *hit);
	void ToReco(TRecoVHit *hit);

	ClassDef(TSlimRecoMUV1Hit, 1)
};


#endif
