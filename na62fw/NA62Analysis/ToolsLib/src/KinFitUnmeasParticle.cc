// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-06
//
// --------------------------------------------------------------------

/// \class KinFitUnmeasParticle
/// \Brief
/// Class for an unmeasured particle in NA62KinFit
/// \EndBrief
/// \Detailed
/// This class is used in NA62KinFit for "unmeasured" particles, 
/// i.e. to obtain particle parameters from fit, decreasing its degrees of freedom.
/// The initial values of decay vertex position and momentum has to be passed to this class
/// when the object is created.
/// The (default) inital error for each parameter is 1e+6, in order to let these values "free" to variate.
///
/// An example of use is given below.
/// \code
/// #include "KinFitUnmeasParticle.hh"
/// ...
/// KinFitUnmeasParticle unmeasPart(InitVtx, InitMom);
/// 
/// Fitter Fit;
/// ...
/// Fit.AddUnmeasPart(unmeasPart);
/// ....
/// \endcode
/// \author Michele Corvino (corvino@na.infn.it)
/// \EndDetailed
#include "KinFitUnmeasParticle.hh"
#include <iostream>
#include <stdlib.h>

using namespace std;

KinFitUnmeasParticle::KinFitUnmeasParticle(){
}


KinFitUnmeasParticle::~KinFitUnmeasParticle(){
	delete InitCovMatrix;
	delete FitCovMatrix;	
}


KinFitUnmeasParticle::KinFitUnmeasParticle(TVector3 Mom, TVector3 cda, double mass){
	SetInitVertex(cda);
	SetInitMomentum(Mom);
	fMass = mass;
	SetEnergy(Mom);
	SetCovMatrix();
}

void KinFitUnmeasParticle::SetInitVertex(TVector3 cda){
	InitParameters.push_back(cda.X());
	InitParameters.push_back(cda.Y());
	InitParameters.push_back(cda.Z());
}

void KinFitUnmeasParticle::SetInitMomentum(TVector3 Mom){
	InitParameters.push_back(Mom.X());
	InitParameters.push_back(Mom.Y());
	InitParameters.push_back(Mom.Z());
}


void KinFitUnmeasParticle::SetCovMatrix(){
	InitCovMatrix = new TMatrixD(6,6);
	for (int i=0; i<6; i++) (*InitCovMatrix)(i,i) = UnmeasSigma*UnmeasSigma;
	
}

