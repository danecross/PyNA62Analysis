// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-03
//
// --------------------------------------------------------------------

/// \class KinFitChargedParticle
/// \Brief
/// Class for charged particles in NA62KinFit (upstream or downstream)
/// \EndBrief
/// \Detailed
/// This class is used in NA62KinFit to get vertex decay, momentum and covariance matrix
/// of a charged particle starting from GTK or Straw candidate and vertex position.
/// The covariance matrix is computed using GTK resolution or Straw covariance matrix when the variable is defined.
///
/// An example of use is given below.
/// \code
/// #include "KinFitChargedParticle.hh"
/// ...
/// KinFitChargedParticle Kaon(GTKCandi, Vtx);
/// \endcode
/// OR
/// \code
/// KinFitChargedParticle Pion(StrawCandi, Vtx);
/// ...
/// Fitter Fit;
/// ...
/// Fit.AddMeasParticle(Kaon);
/// Fit.AddMeasParticle(Pion);
/// ....
/// \endcode
/// \author Michele Corvino (corvino@na.infn.it)
/// \EndDetailed
#include "KinFitChargedParticle.hh"
#include "TVector3.h"

using namespace std;

/// Default constructor
KinFitChargedParticle::KinFitChargedParticle(){
	
}

/// Default destructor
KinFitChargedParticle::~KinFitChargedParticle(){
	delete InitCovMatrix;
	delete FitCovMatrix;
}

/// Constructor for a charged particle in STRAW (pion)
KinFitChargedParticle::KinFitChargedParticle(TRecoSpectrometerCandidate *fCand, TVector3 cda, double mass){
	SetInitVertex(cda);
	SetInitMomentum(fCand->GetThreeMomentumBeforeMagnet());
	fMass = mass;
	SetEnergy(fCand->GetThreeMomentumBeforeMagnet());	
	SetCovMatrix(fCand);
}

/// Constructor for a kaon starting from its GTK candidate
KinFitChargedParticle::KinFitChargedParticle(TRecoGigaTrackerCandidate *fCand, TVector3 cda){
	SetInitVertex(cda);
	SetInitMomentum(fCand->GetMomentum());
	fMass = MKPLUS;
	SetEnergy(fCand->GetMomentum());
	SetCovMatrix(fCand);
}

/// Initialize parameters of a particle
void KinFitChargedParticle::SetInitVertex(TVector3 cda){
	InitParameters.push_back(cda.X());
	InitParameters.push_back(cda.Y());
	InitParameters.push_back(cda.Z());
}

/// Initialize parameters of a particle
void KinFitChargedParticle::SetInitMomentum(TVector3 Mom){
	InitParameters.push_back(Mom.X());
	InitParameters.push_back(Mom.Y());
	InitParameters.push_back(Mom.Z());
}


/// Build a covariance matrix for a particle starting from STRAW candi fit matrix
void KinFitChargedParticle::SetCovMatrix(TRecoSpectrometerCandidate *STRAWCandi){
	Double_t Mom = STRAWCandi->GetMomentum();
	Double_t InvMom = 1/Mom;
	
	Double_t Energy = Mom*Mom+MPIPLUS*MPIPLUS;
	Energy = TMath::Sqrt(Energy);
	
	Double_t SlopeX = STRAWCandi->GetSlopeXBeforeMagnet();
	Double_t SlopeY = STRAWCandi->GetSlopeYBeforeMagnet();
	Double_t R = SlopeX*SlopeX+SlopeY*SlopeY+1;
	R = TMath::Sqrt(R);
	
	Double_t dPxdSlopeX = SlopeY*SlopeY+1;
	dPxdSlopeX = dPxdSlopeX/TMath::Power(R,3);
	dPxdSlopeX = dPxdSlopeX/InvMom;
	
	Double_t dPxdSlopeY = -SlopeX*SlopeY;
	dPxdSlopeY = dPxdSlopeY/TMath::Power(R,3);
	dPxdSlopeY = dPxdSlopeY/InvMom;
	
	Double_t dPxdInvMom = -SlopeX/R;
	dPxdInvMom = dPxdInvMom/TMath::Power(InvMom,2);
	
	Double_t dPydSlopeX = dPxdSlopeY;
	
	Double_t dPydSlopeY = SlopeX*SlopeX+1;
	dPydSlopeY = dPydSlopeY/TMath::Power(R,3);
	dPydSlopeY = dPydSlopeY/InvMom;
	
	Double_t dPydInvMom = -SlopeY/R;
	dPydInvMom = dPydInvMom/TMath::Power(InvMom,2);
	
	Double_t dPzdSlopeX = -SlopeX/TMath::Power(R,3);
	dPzdSlopeX = dPzdSlopeX/InvMom;
	
	Double_t dPzdSlopeY = -SlopeY/TMath::Power(R,3);
	dPzdSlopeY = dPzdSlopeY/InvMom;
	
	Double_t dPzdInvMom = -1/R;
	dPzdInvMom = dPzdInvMom/TMath::Power(InvMom,2);

	TMatrixD A(6,6);
	
	A(0,0) = 1;
	A(1,1) = 1;
	A(2,2) = 1;
	A(3,3) = dPxdSlopeX;
	A(3,4) = dPxdSlopeY;
	A(3,5) = dPxdInvMom;
	A(4,3) = dPydSlopeX;
	A(4,4) = dPydSlopeY;
	A(4,5) = dPydInvMom;
	A(5,3) = dPzdSlopeX;
	A(5,4) = dPzdSlopeY;
	A(5,5) = dPzdInvMom;
	
	
	TMatrixD AT(TMatrixD::kTransposed, A);
	
	TMatrixD V(6,6);
	V(0,0) = fSigmaXVert*fSigmaXVert;
	V(1,1) = fSigmaYVert*fSigmaYVert;
	V(2,2) = fSigmaZVert*fSigmaZVert;
	V(3,3) = STRAWCandi->GetCovariance(0,0);
	V(3,4) = STRAWCandi->GetCovariance(0,1);
	V(3,5) = STRAWCandi->GetCovariance(0,4);
	V(4,3) = V(3,4);
	V(4,4) = STRAWCandi->GetCovariance(1,1);
	V(4,5) = STRAWCandi->GetCovariance(1,4);
	V(5,3) = V(3,5);
	V(5,4) = V(4,5);
	V(5,5) = STRAWCandi->GetCovariance(4,4);


	
	TMatrixD VAT(V,TMatrixD::kMult,AT);
	InitCovMatrix = new TMatrixD(A,TMatrixD::kMult,VAT);
	
		
}

/// Build a covariance matrix for a particle starting from GTK resolution
void KinFitChargedParticle::SetCovMatrix( TRecoGigaTrackerCandidate *GTKCandi){
	Double_t Sigma2Slope;	
	Double_t Sigma2Mom;
	Double_t SlopeX, SlopeY, Mom;
	
	Sigma2Slope = 256E-12;  // 16 urad resolution
    Sigma2Mom = 22500.	 ;  // 0.2% res on 70 Gev/c
   
    SlopeX = GTKCandi->GetMomentum().X()/GTKCandi->GetMomentum().Z();
    SlopeY = GTKCandi->GetMomentum().Y()/GTKCandi->GetMomentum().Z();
    Mom = GTKCandi->GetMomentum().Mag();
	
	Double_t R = SlopeX*SlopeX+SlopeY*SlopeY+1;
	R = TMath::Sqrt(R);
	
	Double_t dPxdSlopeX = SlopeY*SlopeY+1;
	dPxdSlopeX = dPxdSlopeX/TMath::Power(R,3);
	dPxdSlopeX = dPxdSlopeX*Mom;
	
	Double_t dPxdSlopeY = -SlopeX*SlopeY;
	dPxdSlopeY = dPxdSlopeY/TMath::Power(R,3);
	dPxdSlopeY = dPxdSlopeY*Mom;
	
	Double_t dPxdMom = SlopeX/R;
	
	Double_t dPydSlopeX = dPxdSlopeY;
	
	Double_t dPydSlopeY = SlopeX*SlopeX+1;
	dPydSlopeY = dPydSlopeY/TMath::Power(R,3);
	dPydSlopeY = dPydSlopeY*Mom;
	
	Double_t dPydMom = SlopeY/R;
	
	Double_t dPzdSlopeX = -SlopeX/TMath::Power(R,3);
	dPzdSlopeX = dPzdSlopeX*Mom;
	
	Double_t dPzdSlopeY = -SlopeY/TMath::Power(R,3);
	dPzdSlopeY = dPzdSlopeY*Mom;
	
	Double_t dPzdMom = 1/R;
	
	TMatrixD A(6,6);
	
	A(0,0) = 1;
	A(1,1) = 1;
	A(2,2) = 1;
	A(3,3) = dPxdSlopeX;
	A(3,4) = dPxdSlopeY;
	A(3,5) = dPxdMom;
	A(4,3) = dPydSlopeX;
	A(4,4) = dPydSlopeY;
	A(4,5) = dPydMom;
	A(5,3) = dPzdSlopeX;
	A(5,4) = dPzdSlopeY;
	A(5,5) = dPzdMom;
	
	TMatrixD V(6,6);
	V(0,0) = fSigmaXVert*fSigmaXVert;
	V(1,1) = fSigmaYVert*fSigmaYVert;
	V(2,2) = fSigmaZVert*fSigmaZVert;
	V(3,3) = Sigma2Slope;
	V(4,4) = Sigma2Slope;
	V(5,5) = Sigma2Mom;
	
	TMatrixD AT(TMatrixD::kTransposed,A);
	TMatrixD VAT(V,TMatrixD::kMult,AT);
	InitCovMatrix = new TMatrixD(A,TMatrixD::kMult,VAT);
	

}
