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
// Modified (MUV -> MUV2) Rainer Wanke              2010-11-26
//
//
// Modified Mario Vormstein (mario.vormstein@cern.ch)  2011-02-01
//
// Modified Riccardo Aliberti (riccardo.aliberti@cern.ch)  2014-03-22
// --------------------------------------------------------------------
#include "MUV2SD.hh"
#include "MUV2Hit.hh"
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
#include "MCTruthTrackInformation.hh"
#include "MUV2GeometryParameters.hh"


// Constructor
MUV2SD::MUV2SD( G4String name, G4String colName ) :
  G4VSensitiveDetector(name),
  fCollection(nullptr),
  fBirksConstant(0),
  HCID(0)
{
    G4String HCname;
    collectionName.insert( HCname = colName );
    
	for (G4int i = 0; i < 100; i++) {
        fHitContainer[i] = 0;
    }
}

// Destructor
MUV2SD::~MUV2SD()
{
	for (G4int i = 0; i < 100; i++) {
		if (fHitContainer[i] != 0) {
			delete fHitContainer[i];
		}
	}
}

// This function will be called automatically at the beginning of each event
void MUV2SD::Initialize( G4HCofThisEvent *HCE )
{
    static int HCID = -1;
    fCollection   = new MUV2HitsCollection( SensitiveDetectorName, collectionName[0] );
    verboseLevel = 0;
    if ( HCID < 0 )
        HCID = GetCollectionID(0);
    HCE->AddHitsCollection( HCID, fCollection );
    
    
    
	ReadGeometryParameters();
}

void MUV2SD::ReadGeometryParameters() {
	// Read all the geometrical parameters and copy them to private members
	MUV2GeometryParameters* GeoPars = MUV2GeometryParameters::GetInstance();
	fBirksConstant = GeoPars->GetBirksConstant();
    
}


G4bool MUV2SD::ProcessHits(G4Step*aStep,G4TouchableHistory*)
{
    
    G4double edep = aStep->GetTotalEnergyDeposit();
    if(edep==0.) return false;
    const G4TouchableHistory* TouchableHistory = static_cast<const G4TouchableHistory*>((aStep->GetPreStepPoint()->GetTouchable()));
    
    G4int CopyNo  = TouchableHistory->GetVolume(0)->GetCopyNo();

    G4int TrackID = aStep->GetTrack()->GetTrackID();
    
    G4ThreeVector HitPosition = aStep->GetPreStepPoint()->GetPosition();
    
    G4double stepl = aStep->GetStepLength();
    
    G4int    ChannelID     = CopyNo % 100;
    G4double PositionOfHit = 0;
    G4int    Orientation   = -1;

  	if (ChannelID < 50) {//channels in layer of horizontal strips
  		PositionOfHit = HitPosition[1];
  		Orientation = 1;
        
  	} else {
  		PositionOfHit = HitPosition[0];
  		Orientation = 0;
  	}
    
    
	// Apply Birks effect correction to energy deposition
	G4double dEdX = edep / stepl;
    edep = (dEdX / (1 + (fBirksConstant * dEdX))) * stepl;
    
    if (fHitContainer[ChannelID] == 0)
    {
        fHitContainer[ChannelID] = new MUV2HitContainer(HitPosition,edep,aStep->GetPreStepPoint()->GetGlobalTime(),PositionOfHit,CopyNo,ChannelID,Orientation,TrackID);
    }
    else{
        fHitContainer[ChannelID]->AddData(HitPosition,edep,aStep->GetPreStepPoint()->GetGlobalTime(),PositionOfHit,CopyNo,ChannelID,Orientation,TrackID);
    }
    
    
    return true;
}

// This function will be called automatically at the end of each event
void MUV2SD::EndOfEvent( G4HCofThisEvent* )
{
    
	// Comment in for more Output
    
    /*
     G4int NbHits = fCollection->entries();
     
     G4cout << "\n-------->Hits fCollection: in this event they are " << NbHits
     << " hits in the MUV2: " << G4endl;
     //  for (G4int i=0;i<NbHits;i++) (*fCollection)[i]->Print();
     */
    
    
	for (G4int i = 0; i < 100; i++) {
		if (fHitContainer[i] != 0) {
			fHitContainer[i]->InsertCollection(fCollection);
			fHitContainer[i]->Clear();
		}
	}
    
}

void MUV2SD::clear()
{} 

void MUV2SD::DrawAll()
{} 

void MUV2SD::PrintAll()
{} 





