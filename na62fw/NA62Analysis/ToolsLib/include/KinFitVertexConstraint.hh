#ifndef KINFITVERTEXCONSTRAINT_HH
#define KINFITVERTEXCONSTRAINT_HH

#include "KinFitConstraint.hh"

class KinFitVertexConstraint: public KinFitConstraint{
	
	public:
	KinFitVertexConstraint();
	~KinFitVertexConstraint();
	KinFitVertexConstraint(const KinFitParticle &Gamma1, const KinFitParticle &Gamma2);
	
	void SetDerivative();
	void SetDerivative(TMatrixD *Alpha0);
	void SetdVector(const KinFitParticle &Gamma1, const KinFitParticle &Gamma2);
	void SetdVector(TMatrixD *Alpha0);
	
	protected:
	private:	
	
};

#endif

