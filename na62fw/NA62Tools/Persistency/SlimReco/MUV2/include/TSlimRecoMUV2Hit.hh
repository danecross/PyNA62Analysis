#ifndef TSLIMRECOMUV2HIT_HH
#define TSLIMRECOMUV2HIT_HH
#include "TSlimRecoMUVHit.hh"
#include <TVector3.h>

class TSlimRecoMUV2Hit : public TSlimRecoMUVHit {

public:
	TSlimRecoMUV2Hit ()               {}
	explicit TSlimRecoMUV2Hit (TRecoVHit *hit);
	virtual ~TSlimRecoMUV2Hit ()      {}

	Int_t GetQuadrant ()              const;
	Float_t GetScintillatorPosition() const;
	Float_t GetEnergy ()              const { return GetCharge()/1.12; }
    Int_t GetScintillatorNumber ()    const;
    Int_t GetSide ()                  const;
    TVector3 GetPosition ()           const;

	void FromReco(TRecoVHit *hit);
	void ToReco(TRecoVHit *hit);

    ClassDef(TSlimRecoMUV2Hit, 1)
};


#endif
