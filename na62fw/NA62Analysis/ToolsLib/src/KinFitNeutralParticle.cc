// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-03
//
// --------------------------------------------------------------------

/// \class KinFitUnmeasParticle
/// \Brief
/// Class for neutral particles in NA62KinFit (LKr clusters)
/// \EndBrief
/// \Detailed
/// This class is used in NA62KinFit to get vertex decay, momentum and covariance matrix
/// of a neutral particle (photon) starting from LKr candidate and vertex position.
/// The covariance matrix is computed using information from 
/// "The beam and detector for the NA48 neutral kaon CP violation experiment at CERN"
/// V.Fanti et al. ,  Nuclear Instruments and Methods in Physics Research A 574 (2007), page 453
/// 
///
/// An example of use is given below.
/// \code
/// #include "KinFitNeutralParticle.hh"
/// ...
/// KinFitNeutralParticle Gamma(LKrCandi, Vtx);
/// 
/// Fitter Fit;
/// ...
/// Fit.AddMeasParticle(Gamma);
/// ....
/// \endcode
/// \author Michele Corvino (corvino@na.infn.it)
/// \EndDetailed

#include "KinFitNeutralParticle.hh"

using namespace std;

KinFitNeutralParticle::KinFitNeutralParticle(){
	
}

KinFitNeutralParticle::~KinFitNeutralParticle(){
	delete InitCovMatrix;
	delete FitCovMatrix;
}

KinFitNeutralParticle::KinFitNeutralParticle(TRecoLKrCandidate* fCand, TVector3 PiKVertex){
	double clEnergy = fCand->GetClusterEnergy();
	TVector3 Mom;
	Mom.SetXYZ(fCand->GetClusterX(), fCand->GetClusterY(), ZLKR);
	Mom = Mom - PiKVertex;
	Mom.SetMag(clEnergy);
	fMass = 0.;
	SetEnergy(Mom);
	SetInitVertex(PiKVertex);
	SetInitMomentum(Mom);
	SetCovMatrix(fCand, PiKVertex);
}

void KinFitNeutralParticle::SetInitVertex(TVector3 cda){
	InitParameters.push_back(cda.X());
	InitParameters.push_back(cda.Y());
	InitParameters.push_back(cda.Z());
}

void KinFitNeutralParticle::SetInitMomentum(TVector3 Mom){
	InitParameters.push_back(Mom.X());
	InitParameters.push_back(Mom.Y());
	InitParameters.push_back(Mom.Z());
}


void KinFitNeutralParticle::SetCovMatrix(TRecoLKrCandidate *fCand, TVector3 ChargedVertex){
	Double_t SigmaCl;
	Double_t Energy;
	Double_t SigE;
	Double_t temp1, temp2;
	TVector3 Dir;
	TVector3 SigmaDir;	
	
	
	/// computing sigma energy
		
	Energy = fCand->GetClusterEnergy();
	/// Energy in GeV
	Energy = Energy/1000;
	temp1 = 0.42/Energy;
	SigmaCl = RSS(temp1, 0.06);
	/// sigmaCl (error on XY cluster positions) is measured in cm, converting to mm
	SigmaCl = SigmaCl*10;
				
	temp1 = 0.032/TMath::Sqrt(Energy);
	temp2 = 0.09/Energy;
	SigE = RSS(temp1, temp2, 0.0042);

	/// Energy in MeV again
	Energy=Energy*1000;
	SigE = SigE*Energy;
	
	/// computing sigma x,y
	Dir.SetXYZ(fCand->GetClusterX(), fCand->GetClusterY(), ZLKR);
		
	Dir = Dir - ChargedVertex;
	
	Double_t R = Dir.Mag();
	Double_t R3 = TMath::Power(R,3);
	
	TMatrixD V(6,6);
	V(0,0) = fSigmaXVert*fSigmaXVert;
	V(1,1) = fSigmaYVert*fSigmaYVert;
	V(2,2) = fSigmaZVert*fSigmaZVert;
	V(3,3) = SigmaCl*SigmaCl;
	V(4,4) = SigmaCl*SigmaCl;
	V(5,5) = SigE*SigE;
	
	/// derivative of (Xv,Yv,Zv,Px,Py,Pz,E) wrt (Xv,Yv,Zv,Xcl,Ycl,E)
	TMatrixD A(6,6);
	A(0,0) = 1;
	A(1,1) = 1;
	A(2,2) = 1;
	A(3,0) = -Energy/R3*(Dir.Y()*Dir.Y()+Dir.Z()*Dir.Z());
	A(3,1) = Energy/R3*Dir.Y()*Dir.X();
	A(3,2) = Energy/R3*Dir.Z()*Dir.X();
	A(3,3) = Energy/R3*(Dir.Y()*Dir.Y()+Dir.Z()*Dir.Z());
	A(3,4) = -Energy/R3*Dir.Y()*Dir.X();
	A(3,5) = Dir.X()/R;
	A(4,0) = Energy/R3*Dir.X()*Dir.Y();
	A(4,1) = -Energy/R3*(Dir.X()*Dir.X()+Dir.Z()*Dir.Z());
	A(4,2) = Energy/R3*Dir.Z()*Dir.Y();
	A(4,3) = -Energy/R3*Dir.X()*Dir.Y();
	A(4,4) = Energy/R3*(Dir.X()*Dir.X()+Dir.Z()*Dir.Z());
	A(4,5) = Dir.Y()/R;
	A(5,0) = Energy/R3*Dir.X()*Dir.Z();
	A(5,1) = Energy/R3*Dir.Y()*Dir.Z();
	A(5,2) = -Energy/R3*(Dir.X()*Dir.X()+Dir.Y()*Dir.Y());
	A(5,3) = -Energy/R3*Dir.X()*Dir.Z();
	A(5,4) = -Energy/R3*Dir.Y()*Dir.Z();
	A(5,5) = Dir.Z()/R;
	
	
	TMatrixD AT(TMatrixD::kTransposed, A);
	TMatrixD VAT(V, TMatrixD::kMult, AT);
	InitCovMatrix = new TMatrixD(A, TMatrixD::kMult, VAT);
	
	
}

Double_t KinFitNeutralParticle::RSS(Double_t a, Double_t b){
	a = TMath::Power(a,2);
	b = TMath::Power(b,2);
	a = a+b;
	return TMath::Sqrt(a);
}

Double_t KinFitNeutralParticle::RSS(Double_t a, Double_t b, Double_t c){
	a = TMath::Power(a,2);
	b = TMath::Power(b,2);
	c = TMath::Power(c,2);
	a = a+b+c;
	return TMath::Sqrt(a);
}
