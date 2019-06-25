// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-03
//
// --------------------------------------------------------------------

/// \class KinFitVertexContraint
/// \Brief
/// Class for a (neutral) vertex constraintin NA62KinFit
/// \EndBrief
/// \Detailed
/// This class is used in NA62KinFit to constraint the production position of two photons
/// coming from Pi0 decay. 
/// Each particle is parametrized as a 6 dimensional vector (Xv,Yv,Zv,Px,Py,Pz)
/// where the first three variables are Kaon decay vertex position and the last three ones are particle momentum components.
/// For photon candidate, obtained from a LKr cluster, the momentum is correlated to the decay vertex position so that the kinematic fit
/// algorithm can adjust these values.
/// Use this constraint to ensure that decay position is the same for the two photons when the fit is converged.
/// Vertex constraint equations:
/// 
///	Xv_1-Xv_2 = 0
///	Yv_1-Yv_2 = 0
///	Zv_1-Zv_2 = 0
///
/// An example of use is given below.
/// \code
/// #include "KinFitVertexConstraint.hh"
/// ...
/// KinFitVertexConstraint vtxcontr(KinFitNeutralParticle Gamma1, KinFitNeutralParticle Gamma2);
/// Fitter Fit;
/// ...
/// Fit.AddConstraint(vtxconstr);
/// ....
/// \endcode
/// \author Michele Corvino (corvino@na.infn.it)
/// \EndDetailed



#include "KinFitVertexConstraint.hh"

using namespace std;

KinFitVertexConstraint::KinFitVertexConstraint(){
	
}

KinFitVertexConstraint::~KinFitVertexConstraint(){
	delete Derivative;
	delete dVector;
}

KinFitVertexConstraint::KinFitVertexConstraint(const KinFitParticle &Gamma1, const KinFitParticle &Gamma2){
	NEq = 3;
	fConstraintName="Neutral Vertex Constraint";
	SetDerivative();
	SetdVector(Gamma1, Gamma2);	
}

void KinFitVertexConstraint::SetDerivative(){
	Derivative = new TMatrixD(NEq,NParticles*NPARTPARAMS);
	(*Derivative)(0,12) = 1;
	(*Derivative)(0,18) = -1;
	(*Derivative)(1,13) = 1;
	(*Derivative)(1,19) = -1;
	(*Derivative)(2,14) = 1;
	(*Derivative)(2,20) = -1;
}

void KinFitVertexConstraint::SetDerivative(TMatrixD* /*Alpha0*/){
	SetDerivative();
}

void KinFitVertexConstraint::SetdVector(const KinFitParticle &Gamma1, const KinFitParticle &Gamma2){
	dVector = new TMatrixD(NEq,1);
	for(int i=0; i<3; i++){
		(*dVector)(i,0) = Gamma1.InitParameters.at(i)-Gamma2.InitParameters.at(i);
	}
}

void KinFitVertexConstraint::SetdVector(TMatrixD *Alpha0){
	dVector = new TMatrixD(NEq,1);
	(*dVector)(0,0) = (*Alpha0)(12,0)-(*Alpha0)(18,0);
	(*dVector)(1,0) = (*Alpha0)(13,0)-(*Alpha0)(19,0);
	(*dVector)(2,0) = (*Alpha0)(14,0)-(*Alpha0)(20,0);
}

