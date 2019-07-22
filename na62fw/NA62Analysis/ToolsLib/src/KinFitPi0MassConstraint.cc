// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-05
//
// --------------------------------------------------------------------

/// \class KinFitUnmeasParticle
/// \Brief
/// Class for Pi0 mass constraint in NA62KinFit
/// \EndBrief
/// \Detailed
/// This class is used in NA62KinFit to constraint the invariant mass of two photon to be 
/// the Pi0 mass.
///
/// An example of use is given below.
/// \code
/// #include "KinFitPi0MConstraint.hh"
/// ...
/// KinFitPi0MConstraint Pi0MConstr(Gamma1, Gamma2);
/// 
/// Fitter Fit;
/// ...
/// Fit.AddConstraint(Pi0MConstr);
/// ....
/// \endcode
/// \author Michele Corvino (corvino@na.infn.it)
/// \EndDetailed

#include "KinFitPi0MassConstraint.hh"

using namespace std;

KinFitPi0MassConstraint::KinFitPi0MassConstraint(){
}

KinFitPi0MassConstraint::~KinFitPi0MassConstraint(){
	delete Derivative;
	delete dVector;
}

KinFitPi0MassConstraint::KinFitPi0MassConstraint(const KinFitParticle &gamma1, const KinFitParticle &gamma2){
	NEq = 1;
	fConstraintName="Pi0 Mass Constraint";
	SetDerivative(gamma1, gamma2);
	SetdVector(gamma1,gamma2);
}

void KinFitPi0MassConstraint::SetDerivative(const KinFitParticle &gamma1, const KinFitParticle &gamma2){
	Derivative = new TMatrixD(NEq,NParticles*NPARTPARAMS);
	TVector3 p1, p2;
	p1.SetXYZ(gamma1.InitParameters.at(3), gamma1.InitParameters.at(4), gamma1.InitParameters.at(5));
	p2.SetXYZ(gamma2.InitParameters.at(3), gamma2.InitParameters.at(4), gamma2.InitParameters.at(5));
	
	(*Derivative)(0,15) = 2*p2.Mag()*p1.X()/p1.Mag();
	(*Derivative)(0,15) -= 2*p2.X();

	(*Derivative)(0,16) = 2*p2.Mag()*p1.Y()/p1.Mag();
	(*Derivative)(0,16) -= 2*p2.Y();
	
	(*Derivative)(0,17) = 2*p2.Mag()*p1.Z()/p1.Mag();
	(*Derivative)(0,17) -= 2*p2.Z();
	
	(*Derivative)(0,21) = 2*p1.Mag()*p2.X()/p2.Mag();
	(*Derivative)(0,21) -= 2*p1.X();
	
	(*Derivative)(0,22) = 2*p1.Mag()*p2.Y()/p2.Mag();
	(*Derivative)(0,22) -= 2*p1.Y();
	
	(*Derivative)(0,23) = 2*p1.Mag()*p2.Z()/p2.Mag();
	(*Derivative)(0,23) -= 2*p1.Z();
	
}

void KinFitPi0MassConstraint::SetdVector(const KinFitParticle &gamma1, const KinFitParticle &gamma2){
	dVector = new TMatrixD(NEq,1);
	TVector3 p1, p2;
	p1.SetXYZ(gamma1.InitParameters.at(3), gamma1.InitParameters.at(4), gamma1.InitParameters.at(5));
	p2.SetXYZ(gamma2.InitParameters.at(3), gamma2.InitParameters.at(4), gamma2.InitParameters.at(5));
	
	
	(*dVector)(0,0) = 2*p1.Mag()*p2.Mag();
	(*dVector)(0,0) -= 2*p1*p2;
	(*dVector)(0,0) -= MPIZERO*MPIZERO;
	
}

void KinFitPi0MassConstraint::SetDerivative(TMatrixD *Alpha0){
	Derivative = new TMatrixD(NEq,NParticles*NPARTPARAMS);
	TVector3 p1, p2;
	p1.SetXYZ((*Alpha0)(15,0), (*Alpha0)(16,0), (*Alpha0)(17,0));
	p2.SetXYZ((*Alpha0)(21,0), (*Alpha0)(22,0), (*Alpha0)(23,0));
	
	(*Derivative)(0,15) = 2*p2.Mag()*p1.X()/p1.Mag();
	(*Derivative)(0,15) -= 2*p2.X();
	(*Derivative)(0,16) = 2*p2.Mag()*p1.Y()/p1.Mag();
	(*Derivative)(0,16) -= 2*p2.Y();
	(*Derivative)(0,17) = 2*p2.Mag()*p1.Z()/p1.Mag();
	(*Derivative)(0,17) -= 2*p2.Z();
	
	(*Derivative)(0,21) = 2*p1.Mag()*p2.X()/p2.Mag();
	(*Derivative)(0,21) -= 2*p1.X();
	(*Derivative)(0,22) = 2*p1.Mag()*p2.Y()/p2.Mag();
	(*Derivative)(0,22) -= 2*p1.Y();
	(*Derivative)(0,23) = 2*p1.Mag()*p2.Z()/p2.Mag();
	(*Derivative)(0,23) -= 2*p1.Z();
	
}

void KinFitPi0MassConstraint::SetdVector(TMatrixD *Alpha0){
	dVector = new TMatrixD(NEq,1);
	TVector3 p1, p2;
	p1.SetXYZ((*Alpha0)(15,0), (*Alpha0)(16,0), (*Alpha0)(17,0));
	p2.SetXYZ((*Alpha0)(21,0), (*Alpha0)(22,0), (*Alpha0)(23,0));
	(*dVector)(0,0) = 2*p1.Mag()*p2.Mag();
	(*dVector)(0,0) -= 2*p1*p2;
	(*dVector)(0,0) -= MPIZERO*MPIZERO;
}
