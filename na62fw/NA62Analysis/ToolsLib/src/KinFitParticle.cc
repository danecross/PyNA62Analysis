// --------------------------------------------------------------------
// History:
//
// Created by Michele Corvino (corvino@na.infn.it) 2017-05
//
// --------------------------------------------------------------------

/// \class KinFitParticle
/// \Brief
/// Abstract class for a particle in NA62KinFit
/// \EndBrief
/// \Detailed
/// ///
/// This class is called by KinFitChargedParticle, KinFitNeutralParticle and KinFitParticle.
/// It is an abstract class so do not use it directly but use one of the classes mentioned above.
/// ///
/// \author Michele Corvino (corvino@na.infn.it)
/// \EndDetailed
#include "KinFitParticle.hh"

using namespace std;

KinFitParticle::KinFitParticle() :
        InitCovMatrix(nullptr),
	fMass(0),
	fEnergy(0) {}

KinFitParticle::~KinFitParticle(){}

KinFitParticle::KinFitParticle(TVector3 vtx, TVector3 mom, TMatrixD *V) :
	fMass(0),
	fEnergy(0)
{
	InitParameters.push_back(vtx.X());
	InitParameters.push_back(vtx.Y());
	InitParameters.push_back(vtx.Z());
	InitParameters.push_back(mom.X());
	InitParameters.push_back(mom.Y());
	InitParameters.push_back(mom.Z());
	
	InitCovMatrix = new TMatrixD(NPARTPARAMS,NPARTPARAMS);
	
	(*InitCovMatrix) = (*V);
}


TLorentzVector KinFitParticle::GetInit4Momentum(){
	TLorentzVector out;
	Init3Momentum.SetXYZ(InitParameters.at(3), InitParameters.at(4), InitParameters.at(5));
	SetEnergy(Init3Momentum);
	out.SetVect(Init3Momentum);
	out.SetE(GetEnergy());
	return out;
}

TLorentzVector KinFitParticle::GetFit4Momentum(){
	TLorentzVector out;
	Fit3Momentum.SetXYZ(FitParameters.at(3), FitParameters.at(4), FitParameters.at(5));
	SetEnergy(Fit3Momentum);
	out.SetVect(Fit3Momentum);
	out.SetE(GetEnergy());
	return out;
}

