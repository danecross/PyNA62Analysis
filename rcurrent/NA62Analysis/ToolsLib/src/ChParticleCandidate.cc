// ---------------------------------------------------------------
// \class ChParticleCandidate
// History:
//
// Created by Jacopo Pinzino (jacopo.pinzino@cern.ch) 2016-11-30
//
// ---------------------------------------------------------------

#include "ChParticleCandidate.hh"
#include <iostream>

using namespace std;

ChParticleCandidate::ChParticleCandidate()
{
	Clear();
}

void ChParticleCandidate::Clear()
{	
	ftrack = -1;
	fCharge = 0;                 
	fMomentum = 0.0;               
	fCDA = 0.0;                    
	fEnergyLKr = 0.0;                 
	fEnergyMUV1 = 0.0;                 
	fEnergyMUV2 = 0.0;                 
	fEnergyCAL = 0.0;   
	fEop  = 0.0;              
	fTime = 0.0;    
	fMmiss2 = 0.0;
    MUV3Ass = 0;              
	fVertex.SetXYZ(0.0,0.0,0.0);   
	fMomentumBeforeMagnet.SetXYZ(0.0,0.0,0.0);
	fMomentumAfterMagnet.SetXYZ (0.0,0.0,0.0);
	fPositionBeforeMagnet.SetXYZ(0.0,0.0,0.0);
	fPositionAfterMagnet.SetXYZ (0.0,0.0,0.0);
	fMomentumKaon.SetXYZ (0.0,0.0,0.0);
	fElectronProbabilityCombo = 0.0;
	fMuonProbabilityCombo = 0.0;
	fPionProbabilityCombo = 0.0;
}

