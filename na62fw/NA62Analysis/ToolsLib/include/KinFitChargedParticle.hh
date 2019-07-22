#ifndef KINFITCHARGEDPARTICLE_HH
#define KINFITCHARGEDPARTICLE_HH

#include "KinFitParticle.hh"


class KinFitChargedParticle: public KinFitParticle{
	public:
	
	KinFitChargedParticle();
	~KinFitChargedParticle();
	KinFitChargedParticle(TRecoSpectrometerCandidate *fCand, TVector3 cda, double mass);
	KinFitChargedParticle(TRecoGigaTrackerCandidate *fCand, TVector3 cda);
	void SetInitVertex(TVector3 cda);
	void SetInitMomentum(TVector3 Mom);
	void SetCovMatrix(TRecoSpectrometerCandidate *STRAWCandi);
	void SetCovMatrix( TRecoGigaTrackerCandidate *GTKCandi);
	
	protected:
	
	private:
};
#endif
