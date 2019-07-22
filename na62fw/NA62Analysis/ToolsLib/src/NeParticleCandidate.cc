// ---------------------------------------------------------------
// \class NeParticleCandidate
// History:
//
// Created by Jacopo Pinzino (jacopo.pinzino@cern.ch) 2016-11-30
//
// ---------------------------------------------------------------

#include "NeParticleCandidate.hh"
#include <iostream>

using namespace std;

NeParticleCandidate::NeParticleCandidate()
{
	Clear();
}

void NeParticleCandidate::Clear()
{	             
	fEnergyLKr = 0.0;       
	fTime = 0.0;     
	fPosition.SetXYZ(0.0,0.0,0.0);
}

