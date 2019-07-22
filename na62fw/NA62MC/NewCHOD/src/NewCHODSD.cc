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
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// --------------------------------------------------------------------

/// \class NewCHODSD
/// \Brief
/// NewCHOD sensitive detector: generation of NewCHOD MC hits
/// \EndBrief

#include "NewCHODSD.hh"

NewCHODSD::NewCHODSD (G4String name, G4String colName) :
  G4VSensitiveDetector(name),
  Collection(nullptr),
  nHits(0),
  HCID(0)
{
  G4String HCname;
  collectionName.insert(HCname=colName);
  for (G4int i=0; i<500; i++) HitMap[i] = 0;
}

void NewCHODSD::Initialize (G4HCofThisEvent *HCE) {
  static G4int HCID = -1;
  Collection = new NewCHODHitsCollection(SensitiveDetectorName, collectionName[0]);
  verboseLevel = 0;
  nHits = 0;
  if (HCID<0) HCID = GetCollectionID(0);
  HCE->AddHitsCollection(HCID, Collection);
  for (G4int i=0; i<500; i++) if (HitMap[i]) HitMap[i] = 0;
}

G4bool NewCHODSD::ProcessHits(G4Step *aStep, G4TouchableHistory*) {

  G4double EnergyDeposit = aStep->GetTotalEnergyDeposit();
  if (EnergyDeposit==0.0) return false;
  G4double Time1 = aStep->GetPreStepPoint()->GetGlobalTime();

  //G4String VolumeName    = aStep->GetPreStepPoint()->GetPhysicalVolume()->GetName();
  G4Track* Track         = aStep->GetTrack();
  G4int    TrackID       = Track->GetTrackID();
  G4int    ChannelID     = aStep->GetPreStepPoint()->GetPhysicalVolume()->GetCopyNo();
  G4ThreeVector Position = aStep->GetPreStepPoint()->GetPosition();

  /////////////////////////////////////////////////////////////////////////////////
  // Sum energy deposits in the counter over all steps and all tracks in the event.
  // Therefore a maximum of one hit is generated per counter per event.
  // Hit track ID, position and time are defined by the earliest energy deposit.

  if (HitMap[ChannelID]==0) {
    HitMap[ChannelID] = new NewCHODHit;
    HitMap[ChannelID]->SetChannelID(ChannelID);
    HitMap[ChannelID]->SetTrackID(TrackID);
    HitMap[ChannelID]->SetPosition(Position);
    HitMap[ChannelID]->SetTime(Time1);
    HitMap[ChannelID]->SetEnergy(0.0);
    Collection->insert(HitMap[ChannelID]);
    nHits++;
  }

  // Add energy to the existing hits
  HitMap[ChannelID]->AddEnergy(EnergyDeposit);
  if (Time1 < HitMap[ChannelID]->GetTime()) {
    HitMap[ChannelID]->SetTrackID(TrackID);
    HitMap[ChannelID]->SetPosition(Position);
    HitMap[ChannelID]->SetTime(Time1);
  }

  return true;
}
