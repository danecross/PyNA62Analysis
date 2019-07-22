// Class for abstract constraint in NA62KinFit 
//	Author: Michele Corvino (corvino@na.infn.it)
//
//
#ifndef KINFITCONSTRAINT_HH
#define KINFITCONSTRAINT_HH

#include "KinFitParticle.hh"

using namespace std;

class KinFitConstraint{
	public:
	KinFitConstraint();
	virtual ~KinFitConstraint();
	
	TMatrixD* GetDerivative(){return Derivative;}
	TMatrixD* GetdVector(){return dVector;}
	virtual void SetDerivative(TMatrixD *Alpha0) =0;
	virtual void SetdVector(TMatrixD *Alpha0) =0;
	TMatrixD *Derivative;
	TMatrixD *dVector;
	Int_t NEq;
	Int_t GetNEq(){return NEq;}
	Int_t NParticles=4; // default value of particles used for 4momentum constraintss
	Int_t GetNParticles(){return NParticles;}
	TString fConstraintName;
	protected:
	
	private:
	
};
#endif
