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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2008-02-03
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
#include "CHODSD.hh"
#include "CHODHit.hh"
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

CHODSD::CHODSD (G4String name, G4String colName) :
  G4VSensitiveDetector(name),
  Collection(nullptr),
  nHits(0),
  HCID(0)
{
  G4String HCname;
  collectionName.insert(HCname=colName);
}

CHODSD::~CHODSD() {}

void CHODSD::Initialize (G4HCofThisEvent *HCE) {
  static int HCID = -1;
  Collection = new CHODHitsCollection(SensitiveDetectorName, collectionName[0]);
  verboseLevel = 0;
  nHits = 0;
  if (HCID<0) HCID = GetCollectionID(0);
  HCE->AddHitsCollection (HCID, Collection);
}

G4bool CHODSD::ProcessHits (G4Step *aStep, G4TouchableHistory*) {

  G4double edep = aStep->GetTotalEnergyDeposit();
  if (edep==0.0) return false;
  const G4TouchableHistory* TouchableHistory = static_cast<const G4TouchableHistory*>((aStep->GetPreStepPoint()->GetTouchable()));
  //   G4cout << "Next step edep(MeV) = " << edep/MeV << G4endl;

  G4int TrackID = aStep->GetTrack()->GetTrackID();
  G4VPhysicalVolume* Scintillator = TouchableHistory->GetVolume(0);
  G4int ChannelID = Scintillator->GetCopyNo();

  G4ThreeVector HitPosition = aStep->GetPreStepPoint()->GetPosition();
  
  CHODHit* chodHit = new CHODHit();
  chodHit->SetTrackID(TrackID);
  chodHit->SetChannelID(ChannelID);
  chodHit->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
  chodHit->SetEnergy(edep);
  chodHit->SetPosition(HitPosition);
  Collection->insert( chodHit );
  nHits++;
  
  return true;
}

void CHODSD::EndOfEvent(G4HCofThisEvent*) {}

void CHODSD::clear() {} 

void CHODSD::DrawAll() {} 

void CHODSD::PrintAll() {} 
