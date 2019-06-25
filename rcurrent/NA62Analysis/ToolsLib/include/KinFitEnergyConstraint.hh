#ifndef KINFITENERGYCONSTRAINT_HH
#define KINFITENERGYCONSTRAINT_HH
#include "KinFitConstraint.hh"

class KinFitEnergyConstraint: public KinFitConstraint{
	
	public:
	KinFitEnergyConstraint();
	~KinFitEnergyConstraint();
	KinFitEnergyConstraint(const KinFitParticle &K, const KinFitParticle &Pi, const KinFitParticle &G1, const KinFitParticle &G2);
	explicit KinFitEnergyConstraint(std::vector<KinFitParticle> particles);
	
	
	void SetDerivative(const KinFitParticle &K, const KinFitParticle &Pi, const KinFitParticle &G1, const KinFitParticle &G2);
	void SetDerivative(TMatrixD *Alpha0);
	void SetdVector(const KinFitParticle &K, const KinFitParticle &Pi, const KinFitParticle &G1, const KinFitParticle &G2);
	void SetdVector(TMatrixD *Alpha0);
	void SetDecay(TString s);
	TString GetDecayName(){return fDecayName;}
	void SetMasses();
	
	protected:
	private:
	TString fDecayName;
	Double_t fMasses[5];
	
};
#endif

