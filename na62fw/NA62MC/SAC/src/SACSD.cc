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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 
//	      Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------
#include "SACSD.hh"
#include "SACHit.hh"
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
#include "SACGeometryParameters.hh"

SACSD::SACSD(G4String name,G4String colName)
:G4VSensitiveDetector(name)
{
  G4String HCname;
  collectionName.insert(HCname=colName);
  ReadGeometryParameters();
}

SACSD::~SACSD()
{ delete[] hitArray;}

void SACSD::ReadGeometryParameters()
{
  SACGeometryParameters* GeoPars = SACGeometryParameters::GetInstance();
  fSDnSegmentsX = GeoPars->GetSDnSegmentsX();
  fSDnSegmentsY = GeoPars->GetSDnSegmentsY();
  fSDnSegmentsZ = GeoPars->GetSDnSegmentsZ();
  fHitArraySize = fSDnSegmentsX*fSDnSegmentsY*fSDnSegmentsZ;
  hitArray = new SACHit*[fHitArraySize];

  fSACSimulationMode = GeoPars->GetSACSimulationMode();
  fSACDetectorFrontZPosition = GeoPars->GetSACDetectorFrontZPosition();
  fSACDetectorXLength = GeoPars->GetSACDetectorXLength();
  fSACDetectorYLength = GeoPars->GetSACDetectorYLength();
  fSACDetectorZLength = (G4double)GeoPars->GetNLayers()*
    (GeoPars->GetScintillatorLayerZLength()+GeoPars->GetAbsorberLayerZLength()) +
    2.*GeoPars->GetAluminiumLayerZLength();
  fFiberSpacing = GeoPars->GetFiberSpacing();
  fNFibers = GeoPars->GetNFibers();
}

void SACSD::Initialize(G4HCofThisEvent*HCE)
{
  static int HCID = -1;
  Collection = new SACHitsCollection(SensitiveDetectorName,collectionName[0]);
  verboseLevel = 0;
  nHits=0;
  if(HCID<0)
    { HCID = GetCollectionID(0); }
  HCE->AddHitsCollection( HCID, Collection );

  for(G4int i=0;i<fHitArraySize;i++)
    if(hitArray[i] != 0) hitArray[i]=0;

}

G4bool SACSD::ProcessHits(G4Step*aStep,G4TouchableHistory*)
{

  G4double edep = aStep->GetTotalEnergyDeposit();
  if(edep==0.) return false;
  //G4TouchableHistory* TouchableHistory = (G4TouchableHistory*)(aStep->GetPreStepPoint()->GetTouchable());
  
  
  G4int TrackID = aStep->GetTrack()->GetTrackID();
  
  //G4VPhysicalVolume * Scintillator = TouchableHistory->GetVolume(0);
  
  //G4int ChannelID = Scintillator->GetCopyNo();
  
  G4ThreeVector HitPosition = aStep->GetPreStepPoint()->GetPosition();

  /*
  // Save all energy deposits
  SACHit* sacHit = new SACHit();
  sacHit->SetTrackID(TrackID);
  sacHit->SetChannelID(0);
  sacHit->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
  sacHit->SetEnergy(edep);
  sacHit->SetPosition(HitPosition);
  Collection->insert( sacHit );
  nHits++;
  */

  // Save hits in array
  G4double SACLengthX;
  G4double SACLengthY;
  G4double SACLengthZ;
  if(fSACSimulationMode==2) {
    SACLengthX = (G4double)fNFibers*fFiberSpacing;
    SACLengthY = (G4double)fNFibers*fFiberSpacing;
    SACLengthZ = fSACDetectorZLength;
  } else {
    SACLengthX = fSACDetectorXLength;
    SACLengthY = fSACDetectorYLength;
    SACLengthZ = fSACDetectorZLength;
  }
	  G4double stepX = SACLengthX/(G4double)fSDnSegmentsX;
  G4double stepY = SACLengthY/(G4double)fSDnSegmentsY;
  G4double stepZ = SACLengthZ/(G4double)fSDnSegmentsZ;

  G4int iHit = (G4int)((HitPosition.x()+SACLengthX/2.)/stepX);
  G4int jHit = (G4int)((HitPosition.y()+SACLengthY/2.)/stepY);
  G4int kHit = (G4int)((HitPosition.z()-fSACDetectorFrontZPosition)/stepZ);
  if(iHit<0) iHit=0;
  if(jHit<0) jHit=0;
  if(kHit<0) kHit=0;
  if(iHit>(fSDnSegmentsX-1)) iHit = fSDnSegmentsX-1;
  if(jHit>(fSDnSegmentsY-1)) jHit = fSDnSegmentsY-1;
  if(kHit>(fSDnSegmentsZ-1)) kHit = fSDnSegmentsZ-1;
  G4ThreeVector HitPositionToStore = G4ThreeVector(-SACLengthX/2. + (G4double)iHit*stepX + stepX/2.,
						   -SACLengthY/2. + (G4double)jHit*stepY + stepY/2.,
						   fSACDetectorFrontZPosition + (G4double)kHit*stepZ + stepZ/2.);
  //Determine the Channel ID
  G4int ChannelID;
  if(fSDnSegmentsX != 2 || fSDnSegmentsY != 2 ) {
	  //The segmentation doesn't correspond the physical 4 PMTs (2x2)
	  //Keep the linear channel ordering
	  ChannelID = fSDnSegmentsX*fSDnSegmentsY*kHit + fSDnSegmentsX*jHit + iHit;
  } else {
	  //The segmentation corresponds to the physical division of the SAC with 4 PMTs
	  //Use the online/reco channel mapping with a stupid set of IFs...
	  if(HitPosition.x() >= 0 && HitPosition.y() >= 0 ) ChannelID = 0;
	  if(HitPosition.x()  < 0 && HitPosition.y() >= 0 ) ChannelID = 1;
	  if(HitPosition.x()  < 0 && HitPosition.y()  < 0 ) ChannelID = 2;
	  if(HitPosition.x() >= 0 && HitPosition.y()  < 0 ) ChannelID = 3;

  }

  if(hitArray[ChannelID] == 0) {
    hitArray[ChannelID] = new SACHit;
    hitArray[ChannelID]->SetTrackID(TrackID);
    hitArray[ChannelID]->SetChannelID(ChannelID);
    hitArray[ChannelID]->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
    hitArray[ChannelID]->SetTime(0.);
    hitArray[ChannelID]->SetPosition(HitPositionToStore);
    Collection->insert(hitArray[ChannelID] );
    nHits++; 
  }
  hitArray[ChannelID]->SetTime( (aStep->GetPreStepPoint()->GetGlobalTime()* edep +
		  hitArray[ChannelID]->GetEnergy()*	hitArray[ChannelID]->GetTime())
		  / (edep + hitArray[ChannelID]->GetEnergy()  ) );
  hitArray[ChannelID]->AddEnergy(edep);


  return true;
}

void SACSD::EndOfEvent(G4HCofThisEvent*)
{}

void SACSD::clear()
{} 


void SACSD::DrawAll()
{} 

void SACSD::PrintAll()
{} 





