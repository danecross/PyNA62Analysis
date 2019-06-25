#ifndef KINFITMOMENTUMCONSTRAINT_HH
#define KINFITMOMENTUMCONSTRAINT_HH

#include "KinFitConstraint.hh"



class KinFitMomentumConstraint: public KinFitConstraint{
	
	public:
	KinFitMomentumConstraint();
	KinFitMomentumConstraint(const KinFitParticle &k, const KinFitParticle &p1, const KinFitParticle &p2, const KinFitParticle &p3);
	explicit KinFitMomentumConstraint(std::vector<KinFitParticle> Particles);
	~KinFitMomentumConstraint();
	
	void SetDerivative();
	void SetDerivative(TMatrixD *Alpha0);
	void SetdVector(const KinFitParticle &k, const KinFitParticle &p1, const KinFitParticle &p2, const KinFitParticle &p3);
	void SetdVector(std::vector<KinFitParticle> Particles);
	void SetdVector(TMatrixD *Alpha0);
	
	
	protected:
	private:

};
#endif 
