// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-03
//
// --------------------------------------------------------------------
/// \class KinFitMomentumConstraint
/// \Brief
/// Class for three momentum conservation constraint in NA62KinFit 
/// \EndBrief
/// \Detailed
/// This class is used in NA62KinFit to constraint three momentum in a three body decay of a Kaon.
/// Due to the parametrization used, the derivatives are trivial in this constraint.
///
///	3 equations:
/// P_K - P_p1 - P_p2 - P_p3 = 0
/// 
/// An example of use is given below.
/// \code
/// #include "KinFitMomentumConstraint.hh"
/// ...
/// KinFitMomentumConstraint MomConstr(K, p1, p2, p3);
/// ...
/// Fitter Fit;
/// ...
/// Fit.AddConstraint(MomConstr);
/// ....
/// \endcode
/// You can also pass to this class a vector containing all the particles you want to use
/// to perform the fit. Pay attention: Kaon must be the first element of the vector
/// \author Michele Corvino (corvino@na.infn.it)
/// \EndDetailed
#include "KinFitMomentumConstraint.hh"

using namespace std;

KinFitMomentumConstraint::KinFitMomentumConstraint(){
}

KinFitMomentumConstraint::~KinFitMomentumConstraint(){
	delete Derivative;
	delete dVector;
	
}
/// Constructor for 3-body decay
KinFitMomentumConstraint::KinFitMomentumConstraint(const KinFitParticle &k, const KinFitParticle &p1, const KinFitParticle &p2, const KinFitParticle &p3){
	NEq=3;
	NParticles = 4;
	SetDerivative();
	SetdVector(k,p1,p2,p3);
}
/// Constructor for a generic n-body decay 
KinFitMomentumConstraint::KinFitMomentumConstraint(std::vector<KinFitParticle> Particles){
	NEq=3;
	NParticles = Particles.size();
	SetDerivative();
	SetdVector(Particles);
}

void KinFitMomentumConstraint::SetDerivative(){
	Derivative = new TMatrixD(NEq,NParticles*NPARTPARAMS);
	for(int i=0; i<NEq; i++){
		for(int j=0; j<NParticles;j++){
			if(j==0) (*Derivative)(i,(3+j*NPARTPARAMS)+i) = 1;	
			else (*Derivative)(i,(3+j*NPARTPARAMS)+i) = -1;	
		}
	}
	
}

void KinFitMomentumConstraint::SetDerivative(TMatrixD* /*Alpha0*/){
	Derivative = new TMatrixD(NEq,NParticles*NPARTPARAMS);
	for(int i=0; i<NEq; i++){
		(*Derivative)(i,3+i) = 1;
		for(int j=1; j<NParticles; j++){
			(*Derivative)(i,3+(j*NPARTPARAMS)+i) = -1;
		}
	}
}


void KinFitMomentumConstraint::SetdVector(const KinFitParticle &k, const KinFitParticle &p1, const KinFitParticle &p2, const KinFitParticle &p3){
	dVector = new TMatrixD(NEq,1);	
	for(int i=0; i<NEq; i++){
		(*dVector)(i,0)= k.InitParameters.at(i+3)-p1.InitParameters.at(i+3)-p2.InitParameters.at(i+3)-p3.InitParameters.at(i+3);
	}
	
}

void KinFitMomentumConstraint::SetdVector(std::vector<KinFitParticle> Particles){
	dVector = new TMatrixD(NEq,1);
	for(int i=0; i<NEq; i++){
		(*dVector)(i,0) = 0.;
		for(int j=0; j<NParticles;j++){
			if(j==0) (*dVector)(i,0) += Particles.at(j).InitParameters.at(3+i);	
			else (*dVector)(i,0) -= Particles.at(j).InitParameters.at(3+i);	
		}
	}
}

void KinFitMomentumConstraint::SetdVector(TMatrixD *Alpha0){
	dVector = new TMatrixD(NEq,1);	
	for(int i=0; i<NEq; i++){
		for(int j=0; j<NParticles;j++){
			if(j==0) (*dVector)(i,0) = (*Alpha0)(i+3+(j*NPARTPARAMS),0);	
			else (*dVector)(i,0) -= (*Alpha0)(i+3+(j*NPARTPARAMS),0);
		}
	}
}

