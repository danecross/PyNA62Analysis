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
// Modified Riccardo Aliberti                       2014-03-21
// --------------------------------------------------------------------
#include "MUV1SD.hh"
#include "MUV1Hit.hh"
#include "MUV1HitContainer.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4Track.hh"
#include "G4Step.hh"
#include "G4VProcess.hh"
#include "G4ParticleDefinition.hh"
#include "G4VTouchable.hh"
#include "G4TouchableHistory.hh"
#include "G4ios.hh"
#include "G4SDManager.hh"
#include "G4HCofThisEvent.hh"
#include "G4ParticleTypes.hh"
#include "MCTruthTrackInformation.hh"
#include "MUV1GeometryParameters.hh"


#include <cstdio>
#include <stdlib.h>

// Constructor
MUV1SD::MUV1SD(G4String name, G4String colName) :
	G4VSensitiveDetector(name),
	fCollection(nullptr),
	fPMTCollection(nullptr),
	fNHits(0),
	fChannelHits(0),
	HCID(0),
	fBirksConstant(0),
	fPhoton(0)
{
	G4String HCname;
	collectionName.insert(HCname = colName);

	for (G4int i = 0; i < 100; i++) {
			fHitContainer[i] = 0;
	}

	for (G4int i = 0; i < 500; i++)  fHitMap[i] = nullptr;
}

// Destructor
MUV1SD::~MUV1SD() {
	for (G4int i = 0; i < 100; i++) {
		if (fHitContainer[i] != 0) {
			delete fHitContainer[i];
		}
	}
}

// This function will be called automatically at the beginning of each event
void MUV1SD::Initialize(G4HCofThisEvent *HCE) {
	static int HCID = -1;
	fCollection = new MUV1HitsCollection(SensitiveDetectorName,
			collectionName[0]);

	fPMTCollection = new MUV1HitsCollection(SensitiveDetectorName,
			collectionName[0]);

	verboseLevel = 0;
	fNHits = 0;
	fChannelHits = 0;
	if (HCID < 0)
		HCID = GetCollectionID(0);
	HCE->AddHitsCollection(HCID, fCollection);

	for (G4int i = 0; i < 300; i++) {
		if (fHitMap[i] != 0) {
			fHitMap[i] = 0;
		}
	}

	ReadGeometryParameters();
}

void MUV1SD::ReadGeometryParameters() {
	// Read all the geometrical parameters and copy them to private members
	MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();
	fBirksConstant = GeoPars->GetBirksConstant();

}



G4bool MUV1SD::ProcessHits(G4Step*aStep, G4TouchableHistory*) {

		//		//Kill all Photons!
		//		if (aStep->GetTrack()->GetDefinition()->GetParticleName()  == "opticalphoton") { aStep->GetTrack()->SetTrackStatus(fStopAndKill); }

		//Kill low energy Photons!
		if (aStep->GetTrack()->GetDefinition()->GetParticleName() == "opticalphoton"  &&  aStep->GetTrack()->GetTotalEnergy() < 2 * eV) {
			aStep->GetTrack()->SetTrackStatus(fStopAndKill);
			return false;
		}


		G4double edep = aStep->GetTotalEnergyDeposit();
		if (edep == 0.) {	return false;	}

		const G4TouchableHistory* TouchableHistory =	static_cast<const G4TouchableHistory*>( (aStep->GetPreStepPoint()->GetTouchable()));

		G4int TrackID = aStep->GetTrack()->GetTrackID();

		G4int CopyNo = TouchableHistory->GetVolume(0)->GetCopyNo();


		G4int ChannelID = CopyNo % 100;

		G4ThreeVector HitPosition = aStep->GetPreStepPoint()->GetPosition();

		G4double PositionOfHit = 0;
		G4int Orientation = -1;
		if (ChannelID < 50) {
			PositionOfHit = HitPosition[1];
			Orientation = 1;

		} else {
			PositionOfHit = HitPosition[0];
			Orientation = 0;
		}

		G4double stepl = aStep->GetStepLength();

		// Apply Birks effect correction to energy deposition

		G4double dEdX = edep / stepl;

		edep = ( dEdX / (1 + (fBirksConstant * dEdX)) )	* stepl;

		//
		//	  if (fHitMap[ChannelID] == 0)
		//	    {
		//	      fHitMap[ChannelID] = new MUV1Hit;
		//	      fHitMap[ChannelID]->SetScintillatorID(CopyNo);
		//	      fHitMap[ChannelID]->SetChannelID(ChannelID);
		//	      fHitMap[ChannelID]->SetEnergy(0);
		//	      fHitMap[ChannelID]->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
		//	      fHitMap[ChannelID]->SetPosition(HitPosition);
		//	      fHitMap[ChannelID]->SetPositionInScintillator(PositionOfHit);
		//	      fCollection->insert( fHitMap[ChannelID] );
		//	      fChannelHits++;
		//	    }
		//
		//
		//	    fHitMap[ChannelID]->AddEnergy(edep);


		//	 MUV1Hit* muv1Hit = new MUV1Hit();
		//	// muv1Hit->SetTrackID(TrackID);
		//	 muv1Hit->SetChannelID(ChannelID);
		//	 muv1Hit->SetScintillatorID(CopyNo);
		//	 muv1Hit->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
		//	 muv1Hit->SetEnergy(edep);
		//	 //muv1Hit->SetOriginalEnergy(c_energie);
		//	 muv1Hit->SetStepLength(stepl);
		//	 muv1Hit->SetPosition(HitPosition);
		//	 muv1Hit->SetPositionInScintillator(PositionOfHit);
		//	 fCollection->insert( muv1Hit );


		if (fHitContainer[ChannelID] == 0)
		{
			fHitContainer[ChannelID] = new MUV1HitContainer(HitPosition,edep,aStep->GetPreStepPoint()->GetGlobalTime(),PositionOfHit,CopyNo,ChannelID,Orientation,TrackID);
			fChannelHits++;
		}
		else{
			fHitContainer[ChannelID]->AddData(HitPosition,edep,aStep->GetPreStepPoint()->GetGlobalTime(),PositionOfHit,CopyNo,ChannelID,Orientation,TrackID);
		}
		fNHits++;


	return true;
}

// This function will be called automatically at the end of each event
void MUV1SD::EndOfEvent(G4HCofThisEvent*) {

	//Comment in for more output
	/*
	 G4int NbHits = fCollection->entries();

	 G4cout << "\n-------->Hits fCollection: in this event they are " << fNHits
	 << " hits in the MUV1 and  " << fChannelHits << " Hits in each channel"<<G4endl;
	 for (G4int i=0;i<NbHits;i++) (*fCollection)[i]->Print();
	 */

	for (G4int i = 0; i < 100; i++) {
		if (fHitContainer[i] != 0) {
			fHitContainer[i]->InsertCollection(fCollection);
			//delete fHitContainer[i];
            //fHitContainer[i]=0;
            fHitContainer[i]->Clear();
		}
	}


}

void MUV1SD::clear() {
}

void MUV1SD::DrawAll() {
}

void MUV1SD::PrintAll() {
}

