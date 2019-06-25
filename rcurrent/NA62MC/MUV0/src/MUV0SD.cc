// --------------------------------------------------------------------
// History:
//
// Created by Giuseppe Ruggiero 04-09-2012 
// Updated: Evgueni Goudzovski  11-05-2016
//
// --------------------------------------------------------------------

/// \class MUV0SD
/// \Brief
/// MUV0 sensitive detector: generation of MUV0 MC hits
/// \EndBrief

#include "MUV0SD.hh"

MUV0SD::MUV0SD (G4String name, G4String colName) :
  G4VSensitiveDetector(name),
  Collection(nullptr),
  nHits(0),
  HCID(0)
{
  G4String HCname;
  collectionName.insert(HCname=colName);

  for (G4int i=0; i<10; i++) HitMap[i] = 0; // Channel IDs can be 1-9
}

void MUV0SD::Initialize (G4HCofThisEvent *HCE) {
  static G4int HCID = -1;
  Collection = new MUV0HitsCollection(SensitiveDetectorName, collectionName[0]);
  verboseLevel = 0;
  nHits = 0;
  if (HCID<0) HCID = GetCollectionID(0);
  HCE->AddHitsCollection(HCID, Collection);
  for (G4int i=0; i<10; i++) HitMap[i] = 0; // Channel IDs can be 1-9
}

G4bool MUV0SD::ProcessHits (G4Step *aStep, G4TouchableHistory*) {

  G4double EnergyDeposit = aStep->GetTotalEnergyDeposit();
  if (EnergyDeposit==0.0) return false;
  G4double Time1 = aStep->GetPreStepPoint()->GetGlobalTime();

  G4String VolumeName    = aStep->GetPreStepPoint()->GetPhysicalVolume()->GetName();
  G4Track* Track         = aStep->GetTrack();
  G4int    TrackID       = Track->GetTrackID();
  G4int    ChannelID     = aStep->GetPreStepPoint()->GetPhysicalVolume()->GetCopyNo();
  G4ThreeVector Position = aStep->GetPreStepPoint()->GetPosition();

  /////////////////////////////////////////////////////////////////////////////////
  // Sum energy deposits in the counter over all steps and all tracks in the event.
  // Therefore a maximum of one hit is generated per counter per event.
  // Hit track ID, position and time are defined by the earliest energy deposit.

  if (HitMap[ChannelID]==0) {
    HitMap[ChannelID] = new MUV0Hit();
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
