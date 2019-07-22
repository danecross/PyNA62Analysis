// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-06
//
// --------------------------------------------------------------------
#ifndef KINFITUNMEASPARTICLE_HH
#define KINFITUNMEASPARTICLE_HH

#include "KinFitParticle.hh"


class KinFitUnmeasParticle: public KinFitParticle{
	public:
	
	KinFitUnmeasParticle();
	~KinFitUnmeasParticle();
	KinFitUnmeasParticle(TVector3 fVec, TVector3 cda, double mass);
	void SetInitVertex(TVector3 cda);
	void SetInitMomentum(TVector3 Mom);
	void SetCovMatrix();
	
	protected:
	
	private:
	double UnmeasSigma = 1e+6;
};
#endif

