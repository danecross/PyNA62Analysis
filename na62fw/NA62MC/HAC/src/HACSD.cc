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
// Created by Giuseppe Ruggiero 04-09-2012
// 2013-02-11 Spasimir Balev - Update of signal collection
//
// --------------------------------------------------------------------
#include "HACSD.hh"
#include "HACHit.hh"
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

HACSD::HACSD(G4String name,G4String colName) :
  G4VSensitiveDetector(name),
  Collection(nullptr),
  nHits(0),
  HCID(0)
{
  G4String HCname;
  collectionName.insert(HCname=colName);
  ReadGeometryParameters();
}

HACSD::~HACSD()
{delete[] hitArray;}

void HACSD::ReadGeometryParameters()
{
  hitArray = new HACHit*[90];
}

void HACSD::Initialize(G4HCofThisEvent*HCE)
{
  static int HCID = -1;
  Collection = new HACHitsCollection(SensitiveDetectorName,collectionName[0]);
  verboseLevel = 0;
  nHits=0;
  if(HCID<0)
    { HCID = GetCollectionID(0); }
  HCE->AddHitsCollection( HCID, Collection );

  for(G4int i=0;i<90;i++)
    if(hitArray[i] != 0) hitArray[i]=0;
}

G4bool HACSD::ProcessHits(G4Step*aStep,G4TouchableHistory*)
{

  G4double edep = aStep->GetTotalEnergyDeposit();
  if(edep==0.) return false;
  const G4TouchableHistory* TouchableHistory = static_cast<const G4TouchableHistory*>((aStep->GetPreStepPoint()->GetTouchable()));

  G4int TrackID = aStep->GetTrack()->GetTrackID();

  G4VPhysicalVolume * Scintillator = TouchableHistory->GetVolume(0);

  G4int ChannelID = Scintillator->GetCopyNo();

  G4ThreeVector HitPosition = aStep->GetPreStepPoint()->GetPosition();

  /*  
  // Uncomment this if you want to store every energy deposit
  HACHit* hacHit = new HACHit();
  hacHit->SetTrackID(TrackID);
  hacHit->SetChannelID(ChannelID);
  hacHit->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
  hacHit->SetEnergy(edep);
  hacHit->SetPosition(HitPosition);
  Collection->insert( hacHit );
  nHits++;
  */

  if(hitArray[ChannelID] == 0) {
    hitArray[ChannelID] = new HACHit;
    hitArray[ChannelID]->SetTrackID(TrackID);
    hitArray[ChannelID]->SetChannelID(ChannelID);
    hitArray[ChannelID]->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
    hitArray[ChannelID]->SetPosition(HitPosition);
    Collection->insert(hitArray[ChannelID] );
    nHits++; 
  }
  hitArray[ChannelID]->AddEnergy(edep);

  return true;
}

void HACSD::EndOfEvent(G4HCofThisEvent*)
{}

void HACSD::clear()
{} 


void HACSD::DrawAll()
{} 

void HACSD::PrintAll()
{} 





