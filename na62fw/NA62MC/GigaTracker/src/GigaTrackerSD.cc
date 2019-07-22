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
// --------------------------------------------------------------
// History:
//
// 2012-03-09 B.Velghe (bob.velghe@cern.ch)
// - PixelID and StationID: Change TouchableHistory "volume depth" according to the new geometry
//
// 2008-05-06 S.Bifani
// - Fixed a TouchableHistory problem in the pixel & station ID
//
// 2008-04-22 S.Bifani (Simone.Bifani@cern.ch)
// - Added main GTK info to the Hit (pixel & statin ID, energy & time,
//   position, simulated track ID)
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// --------------------------------------------------------------
//
#include "GigaTrackerSD.hh"
#include "GigaTrackerHit.hh"
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

GigaTrackerSD::GigaTrackerSD(G4String name, G4String colName) :
  G4VSensitiveDetector(name),
  Collection(nullptr),
  nHits(0),
  HCID(0)
{

  G4String HCname;
  collectionName.insert(HCname=colName);

}

GigaTrackerSD::~GigaTrackerSD() {}

void GigaTrackerSD::Initialize(G4HCofThisEvent*HCE)
{

  static int HCID = -1;
  Collection = new GigaTrackerHitsCollection(SensitiveDetectorName, collectionName[0]);
  verboseLevel = 0;
  nHits=0;
  if(HCID<0)
    { HCID = GetCollectionID(0); }
  HCE->AddHitsCollection( HCID, Collection );

}

G4bool GigaTrackerSD::ProcessHits(G4Step*aStep, G4TouchableHistory*)
{

  G4double Energy = aStep->GetTotalEnergyDeposit();
  if(Energy == 0.) return false;
  //G4cout << "Next step edep(MeV) = " << edep/MeV << G4endl;

  const G4TouchableHistory* TouchableHistory = static_cast<const G4TouchableHistory*>((aStep->GetPreStepPoint()->GetTouchable()));

  G4int PixelID = TouchableHistory->GetVolume(0)->GetCopyNo(); //Pixels
  G4int StationNo = TouchableHistory->GetVolume(3)->GetCopyNo(); //SensorAssembly

  G4double Time = aStep->GetPreStepPoint()->GetGlobalTime();
  
  G4int TrackID = aStep->GetTrack()->GetTrackID();

  G4ThreeVector Position = aStep->GetPreStepPoint()->GetPosition();

  GigaTrackerHit* GTKHit = new GigaTrackerHit;

  GTKHit->SetEnergy(Energy);
  G4int newPixelID = 199-PixelID%200 + (89-PixelID/200)*200; // pixel remapping to match data
  GTKHit->SetPixelID(newPixelID);
  GTKHit->SetStationNo(StationNo);
  GTKHit->SetTime(Time);
  GTKHit->SetTrackID(TrackID);
  GTKHit->SetPosition(Position);
  
  Collection->insert( GTKHit );
  nHits++;
  
  return true;

}

void GigaTrackerSD::EndOfEvent(G4HCofThisEvent*) {}

void GigaTrackerSD::clear() {} 

void GigaTrackerSD::DrawAll() {} 

void GigaTrackerSD::PrintAll() {} 
