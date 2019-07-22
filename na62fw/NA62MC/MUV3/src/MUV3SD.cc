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

// --------------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 
//	      Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------

/// \class MUV3SD
/// \Brief
/// MUV3 sensitive detector: generation of MUV3 MC hits
/// \EndBrief

#include "MUV3SD.hh"

MUV3SD::MUV3SD (G4String name, G4String colName) :
  G4VSensitiveDetector(name),
  Collection(nullptr),
  nHits(0),
  HCID(0)
{
  G4String HCname;
  collectionName.insert(HCname=colName);

  // Two PMTs per counter, channel IDs: 0-151 and 200-351
  for (G4int i=0; i<360; i++) {
    HitMapScintillator[i] = 0;
    HitMapPMTWindow[i]    = 0;
  }
}

void MUV3SD::Initialize (G4HCofThisEvent *HCE) {
  static G4int HCID = -1;
  Collection = new MUV3HitsCollection(SensitiveDetectorName, collectionName[0]);
  verboseLevel = 0;
  nHits = 0;
  if (HCID<0) HCID = GetCollectionID(0);
  HCE->AddHitsCollection (HCID, Collection);

  // Two PMTs per counter, channel IDs: 0-151 and 200-351
  for (G4int i=0; i<360; i++) {
    HitMapScintillator[i] = 0;
    HitMapPMTWindow[i]    = 0;
  }
}

G4bool MUV3SD::ProcessHits (G4Step *aStep, G4TouchableHistory*) {
  G4double EnergyDeposit = aStep->GetTotalEnergyDeposit();
  if (EnergyDeposit==0.0) return false;
  G4double Time1 = aStep->GetPreStepPoint()->GetGlobalTime();
  //G4double Time2 = aStep->GetPostStepPoint()->GetGlobalTime();

  G4String VolumeName    = aStep->GetPreStepPoint()->GetPhysicalVolume()->GetName();
  G4bool   PMTWindowHit  = VolumeName.contains("PMT"); // Scintillator or PMT hit?
  G4Track* Track         = aStep->GetTrack();
  G4int    TrackID       = Track->GetTrackID();
  G4int    ParticleCode  = Track->GetDefinition()->GetPDGEncoding();
  G4int    ChannelID     = aStep->GetPreStepPoint()->GetPhysicalVolume()->GetCopyNo();
  G4ThreeVector Position = aStep->GetPreStepPoint()->GetPosition();
  
  ////////////////////////////////////////////////
  // Hit due to energy deposit in the scintillator

  if (!PMTWindowHit) {

    // Sum energy deposits in the counter over all steps and all tracks in the event.
    // Therefore, maximum one hit is generated per channel per event.
    // Hit track ID, position and time are defined by the earliest energy deposit.
    // Identical hits are created for the two PMTs watching the scintillator counter.

    // Create a new hit if there is no hit in this tile yet
    if (HitMapScintillator[ChannelID]==0) {
      for (G4int id = ChannelID; id<=ChannelID+200; id+=200) { // two PMTs per tile
	HitMapScintillator[id] = new MUV3Hit;
	HitMapScintillator[id]->SetChannelID(id);
	HitMapScintillator[id]->SetTrackID(TrackID);
	HitMapScintillator[id]->SetPosition(Position);
	HitMapScintillator[id]->SetTime(Time1);
	HitMapScintillator[id]->SetEnergy(0.0);
	HitMapScintillator[id]->SetMuonHit(false);
	Collection->insert(HitMapScintillator[id]);
	nHits++;
      }
    }

    // Add energy to the existing hits
    for (G4int id = ChannelID; id<=ChannelID+200; id+=200) { // two PMTs per tile
      HitMapScintillator[id]->AddEnergy(EnergyDeposit);
      if (Time1 < HitMapScintillator[id]->GetTime()) {
	HitMapScintillator[id]->SetTrackID(TrackID);
	HitMapScintillator[id]->SetPosition(Position);
	HitMapScintillator[id]->SetTime(Time1);
      }
      if (abs(ParticleCode)==13) HitMapScintillator[id]->SetMuonHit(true);
    }
  }

  ///////////////////////////////////////////////////////////////////
  // Hit due to energy deposit in the PMT window: maximum one per PMT
  // Borosilicate glass refractive index: n=1.51 ==> Cherenkov threshold beta = 1/n = 0.66

  else if (Track->GetDefinition()->GetPDGCharge() &&                  // charged particle
	   Track->GetMomentum().mag()/Track->GetTotalEnergy()>0.66) { // above Cherenkov threshold

    if (HitMapPMTWindow[ChannelID]==0) {
      HitMapPMTWindow[ChannelID] = new MUV3Hit;
      HitMapPMTWindow[ChannelID]->SetChannelID(ChannelID);
      HitMapPMTWindow[ChannelID]->SetTrackID(TrackID);
      HitMapPMTWindow[ChannelID]->SetPosition(Position);
      HitMapPMTWindow[ChannelID]->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
      HitMapPMTWindow[ChannelID]->SetEnergy(-999*GeV); // negative energy to distinguish from normal hits
      Collection->insert(HitMapPMTWindow[ChannelID]);
      nHits++;
    }
    if (abs(ParticleCode)==13) HitMapPMTWindow[ChannelID]->SetMuonHit(true);
  }
  return true;
}
