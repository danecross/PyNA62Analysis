//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2008-03-11
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Modified (MUV -> MUV1) Rainer Wanke              2010-11-26
// Modified Mario Vormstein							2011-06-20
//
// --------------------------------------------------------------------
#include "MUV1FiberSD.hh"
#include "MUV1Hit.hh"
#include "MUV1GeometryParameters.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4Track.hh"
#include "G4Step.hh"
#include "G4ParticleDefinition.hh"
#include "G4VTouchable.hh"
#include "G4TouchableHistory.hh"
#include "G4ios.hh"
#include "G4SDManager.hh"
#include "G4HCofThisEvent.hh"
#include "G4ParticleTypes.hh"

#include <cstdio>
#include <stdlib.h>

/// \class MUV1FiberSD
/// \Brief
/// MUV1FiberSD class.
/// \EndBrief
///
/// \Detailed
/// Sensitive class for the fibers. In the full optical simulation used to kill photons below a given threshold, as this will not be detected by PMT
/// \EndDetailed



// Constructor
MUV1FiberSD::MUV1FiberSD(G4String name) :
	G4VSensitiveDetector(name),
	HCID(0),
	fPhotonEnergyCut(0)
{
}

// Destructor
MUV1FiberSD::~MUV1FiberSD() {
	;
}

// This function will be called automatically at the beginning of each event
void MUV1FiberSD::Initialize(G4HCofThisEvent */*HCE*/) {
	MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();
	fPhotonEnergyCut = GeoPars->GetFiberPhotonEnergyCut();

}

G4bool MUV1FiberSD::ProcessHits(G4Step*aStep, G4TouchableHistory*) {

	//Kill low energy Photons!


	if (aStep->GetTrack()->GetDefinition()->GetParticleName()
			== "opticalphoton" && aStep->GetTrack()->GetTotalEnergy() < fPhotonEnergyCut) {
		aStep->GetTrack()->SetTrackStatus(fStopAndKill);

		return true;

	}

	return true;
}

// This function will be called automatically at the end of each event
void MUV1FiberSD::EndOfEvent(G4HCofThisEvent*) {
}

void MUV1FiberSD::clear() {
}

void MUV1FiberSD::DrawAll() {
}

void MUV1FiberSD::PrintAll() {
}

