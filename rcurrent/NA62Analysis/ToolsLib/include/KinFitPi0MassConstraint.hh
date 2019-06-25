// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-06
//
// --------------------------------------------------------------------
#ifndef KINFITPI0MASSCONTSTRAINT_HH
#define KINFITPI0MASSCONSTRAINT_HH

#include "KinFitConstraint.hh"

class KinFitPi0MassConstraint: public KinFitConstraint{
	public:
	
	KinFitPi0MassConstraint();
	KinFitPi0MassConstraint(const KinFitParticle &gamma1, const KinFitParticle &gamma2);
	~KinFitPi0MassConstraint();
	
	void SetDerivative(const KinFitParticle &gamma1, const KinFitParticle &gamma2);
	void SetDerivative(TMatrixD *Alpha0);
	void SetdVector(const KinFitParticle &gamma1, const KinFitParticle &gamma2);
	void SetdVector(TMatrixD *Alpha0);
	
	
	protected:
	
	private:
	
};



#endif
