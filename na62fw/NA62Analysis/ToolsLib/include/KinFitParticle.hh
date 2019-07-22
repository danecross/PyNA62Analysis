//	Abstract class for a Particle in NA62KinFit
//	Author: Michele Corvino (corvino@na.infn.it)
	
// Parametrization of a particle (Xv, Yv, Zv, Px, Py, Pz)
// Covariance Matrix
//
//		S2Xv	SXvYv 	SXvZv 	SXvPx	SXvPy	SXvPz
//				S2Yv	SYvZv	SYvPx 	SYxPy	SYvPz
//						S2Zv	SZvPx	SZvPy	SZvPz
//								S2Px	SPxPy	SPxPz
//										S2Py	SPyPz
//												S2Pz
//

#ifndef KINFITPARTICLE_HH
#define KINFITPARTICLE_HH

#include "Persistency.hh"
#include "TMatrixD.h"
#include "TMatrixDSym.h"
#include "TVectorD.h"
#include "TLorentzVector.h"
#include "TVector3.h"
#include "GeometricAcceptance.hh"
#include "TNamed.h"
#include <vector>
#include <iostream>
#include <stdlib.h>

///number of parameters for each KinFitParticle
#define NPARTPARAMS 6

///physical quantities
#define MKPLUS 493.677
#define MPIZERO 134.9766
#define MPIPLUS 139.57018

using namespace std;

class KinFitParticle{
	public:
	
	KinFitParticle();
	virtual ~KinFitParticle();
	KinFitParticle(TVector3 vtx, TVector3 mom, TMatrixD *V);
	std::vector<double> InitParameters; 								// Initial parameters
	//~ std::vector<double> CurrParameters; 							// Current parameters (not implemented!)
	std::vector<double> FitParameters;									// Fitted parameters
	TMatrixD *InitCovMatrix;											// Initial Covariance Matrix
	TMatrixD *FitCovMatrix = new TMatrixD(NPARTPARAMS,NPARTPARAMS);		// Fitted Covariance Matrix
	TMatrixD* GetCovMatrix(){return InitCovMatrix;}
	TLorentzVector GetInit4Momentum();
	TLorentzVector GetFit4Momentum();
	TVector3 Init3Momentum;
	TVector3 Fit3Momentum;
	void SetEnergy(TVector3 Mom){fEnergy = TMath::Sqrt(Mom.Mag2()+fMass*fMass);}
	double GetEnergy() const {return fEnergy;}
	void SetMass(double m){fMass=m;}
	double fMass;
	double fEnergy;
	double fSigmaXVert = 2;											// Error on x coordinate of vertex (initial value 2)
	double fSigmaYVert = 2;											// Error on y coordinate of vertex (initial value 2)
	double fSigmaZVert = 500;										// Error on z coordinate of vertex (initial value 500)
	double ZLKR = GeometricAcceptance::GetInstance()->GetZLKr();
	//~ protected:
	
	private:
	
	// parameters manipulations
	
};
#endif


