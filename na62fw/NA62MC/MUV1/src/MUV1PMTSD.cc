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
// Modified Mario Vormstein (mario.vormstein@uni-mainz.de)
//
// Sensitive Detector for the PMT
// --------------------------------------------------------------------
#include "MUV1PMTSD.hh"
#include "MUV1Hit.hh"
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
#include "G4Poisson.hh"

#include <cstdio>
#include <stdlib.h>

// Constructor
MUV1PMTSD::MUV1PMTSD(G4String name, G4String colName) :
	G4VSensitiveDetector(name),
	PMTCollection(nullptr),
	nHits(0),
	nChannelHits(0),
	HCID(0),
	fPhoton(0)
{
	G4String HCname;
	collectionName.insert(HCname = colName);

	for(int i=0; i<500; ++i) HitMap[i] = nullptr;
}

// Destructor
MUV1PMTSD::~MUV1PMTSD() {
	;
}

// This function will be called automatically at the beginning of each event
void MUV1PMTSD::Initialize(G4HCofThisEvent *HCE) {
	static int HCID = -1;
	PMTCollection = new MUV1HitsCollection(SensitiveDetectorName, collectionName[0]);
	verboseLevel = 0;
	nHits = 0;
	nChannelHits = 0;
	fPhoton = 0;
	if (HCID < 0)
		HCID = GetCollectionID(0);
	HCE->AddHitsCollection(HCID, PMTCollection);

	for (G4int i = 0; i < 300; i++) {
		if (HitMap[i] != 0) {
			HitMap[i] = 0;
		}
	}

}

G4bool MUV1PMTSD::ProcessHits(G4Step*aStep, G4TouchableHistory*) {
	// G4double edep = aStep->GetTotalEnergyDeposit();
	// if (edep == 0.) return false;

	const G4TouchableHistory* TouchableHistory =	static_cast<const G4TouchableHistory*>( (aStep->GetPreStepPoint()->GetTouchable()));

	G4int CopyNo = TouchableHistory->GetVolume(0)->GetCopyNo();

	G4int TrackID = aStep->GetTrack()->GetTrackID();

	// Full Simulation
	// The Hits in the virtual PMT are mapped to the correct channels
	if (aStep->GetTrack()->GetDefinition()->GetParticleName() == "opticalphoton") {
		//G4cout << "In PMT "<<CopyNo<<" we had just a hit" << G4endl;

		G4int ScintNo = CopyNo % 100;
		G4int VPMTNo = CopyNo / 10000;
		G4int PMTNo;
		if(ScintNo < 50){
			if (VPMTNo == 1 || VPMTNo == 2) {
				PMTNo = 200 + ScintNo;
			} else {
				PMTNo = 100 + ScintNo;
			}
		}
		else{
			if (VPMTNo == 1 || VPMTNo == 2) {
				PMTNo = 100 + ScintNo;
			} else {
				PMTNo = 200 + ScintNo;
			}
		}
		aStep->GetTrack()->SetTrackStatus(fStopAndKill);
		fPhoton++;

		MUV1Hit* muv1Hit = new MUV1Hit();

		muv1Hit->SetTrackID(TrackID);
		muv1Hit->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
		muv1Hit->SetPhotons(PMTNo);
		muv1Hit->SetChannelID(VPMTNo);
		PMTCollection->insert( muv1Hit );
		nHits++;
		return true;

	}

	return true;
}

// This function will be called automatically at the end of each event
void MUV1PMTSD::EndOfEvent(G4HCofThisEvent*) {

	//	G4cout << "In this Event we had " << fPhoton << " hits of PMT" << G4endl;
		fPhoton = 0;


		//Comment in for more output
		/*
		 G4int NbHits = Collection->entries();

		 G4cout << "\n-------->Hits Collection: in this event they are " << nHits
		 << " hits in the MUV1 and  " << nChannelHits << " Hits in each channel"<<G4endl;
		 for (G4int i=0;i<NbHits;i++) (*Collection)[i]->Print();
		 */

}

void MUV1PMTSD::clear() {
}

void MUV1PMTSD::DrawAll() {
}

void MUV1PMTSD::PrintAll() {
}

