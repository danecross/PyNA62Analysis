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
// Geometry corrected by Lubos Bician (Lubos.Bician@cern.ch) 2019-02-22
//   -- Straw.cc modifications by Zuzana required redefinitions
//	of Straw, HV, View, Chamber and Detector physical volumes.
//
// 2008-04-23 A.Sergi  Hits are marked by IDs to identify the Straw Tube
//                      
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------------
#include "SpectrometerSD.hh"
#include "SpectrometerHit.hh"
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

#include "G4Tubs.hh"

SpectrometerSD::SpectrometerSD(G4String name,G4String colName) :
  G4VSensitiveDetector(name),
  Collection(nullptr),
  nHits(0),
  HCID(0)
{
  G4String HCname;
  collectionName.insert(HCname=colName);
}

SpectrometerSD::~SpectrometerSD()
{;}

void SpectrometerSD::Initialize(G4HCofThisEvent* HCE)
{
  static int HCID = -1;
  Collection = new SpectrometerHitsCollection(SensitiveDetectorName,collectionName[0]);
  verboseLevel = 0;
  nHits=0;
  if(HCID<0)
    { HCID = GetCollectionID(0); }
  HCE->AddHitsCollection( HCID, Collection );
}

G4bool SpectrometerSD::ProcessHits(G4Step* aStep, G4TouchableHistory*)
{

  G4double edep = aStep->GetTotalEnergyDeposit();
  if(edep==0.) return false;
  const G4TouchableHistory* TouchableHistory = static_cast<const G4TouchableHistory*>((aStep->GetPreStepPoint()->GetTouchable()));
//   G4cout << "Next step edep(MeV) = " << edep/MeV << G4endl;

  G4VPhysicalVolume * StrawGas = TouchableHistory->GetVolume(0);
  G4VPhysicalVolume * Straw = TouchableHistory->GetVolume(3);
  G4VPhysicalVolume * HalfView = TouchableHistory->GetVolume(4);
  G4VPhysicalVolume * View = TouchableHistory->GetVolume(5);
  G4VPhysicalVolume * Chamber = TouchableHistory->GetVolume(6);
  G4VPhysicalVolume * DetectorRegion = TouchableHistory->GetVolume(7);
  
  G4int StrawID = StrawGas->GetCopyNo();
  G4int HalfViewID = HalfView->GetCopyNo();
  G4int ViewID = View->GetCopyNo();
  G4int ChamberID = Chamber->GetCopyNo();
  G4int TrackID = aStep->GetTrack()->GetTrackID();

  G4ThreeVector TrackDirection = (aStep->GetDeltaPosition());

  G4ThreeVector StrawAxis(0.,1.,0.);
  G4ThreeVector StrawPosition;
  StrawAxis = View->GetObjectRotationValue()*StrawAxis;
  G4ThreeVector TransverseDirection = (StrawAxis.cross(TrackDirection)).unit();

  StrawPosition = View->GetObjectRotationValue()*(Straw->GetObjectTranslation()+HalfView->GetObjectTranslation())
    +View->GetObjectTranslation()+Chamber->GetObjectTranslation()+DetectorRegion->GetObjectTranslation();

  G4ThreeVector HitPosition = aStep->GetPreStepPoint()->GetPosition();
  G4ThreeVector LocalPosition = HitPosition-StrawPosition;

  G4double WireDistance = (TransverseDirection * (LocalPosition * TransverseDirection)).mag();
//   G4cout << "IDs = (" << ChamberID << "," << ViewID << "," << HalfViewID << "," << StrawID << ")" << G4endl
// 	 << "Position = " << StrawPosition << G4endl 
// 	 << "LocalHitPosition = " << LocalPosition << G4endl
// 	 << "Axis = " << StrawAxis << G4endl
// 	 << "HitPosition = " << HitPosition << G4endl 
// 	 << "WireAverageDistance = " << WireDistance << G4endl
// 	 << "Energy = " << edep << G4endl
// 	 << "Time = " << aStep->GetPreStepPoint()->GetGlobalTime() << G4endl
// 	 << "TrackID = " << TrackID << G4endl 
// 	 << "TrackDirection = " << TrackDirection << G4endl 
// 	 << G4endl;

  SpectrometerHit* spectrometerHit = new SpectrometerHit();
  spectrometerHit->SetStrawID(StrawID);
  spectrometerHit->SetHalfViewID(HalfViewID);
  spectrometerHit->SetViewID(ViewID);
  spectrometerHit->SetChamberID(ChamberID);
  spectrometerHit->SetTrackID(TrackID);

  spectrometerHit->SetWireDistance(WireDistance);
  spectrometerHit->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
  spectrometerHit->SetEnergy(edep);
  spectrometerHit->SetPosition(HitPosition);
  spectrometerHit->SetLocalPosition(LocalPosition);

  spectrometerHit->SetDirection(TrackDirection);

  Collection->insert( spectrometerHit );
  nHits++;
  
  return true;
}

void SpectrometerSD::EndOfEvent(G4HCofThisEvent*)
{}

void SpectrometerSD::clear()
{} 


void SpectrometerSD::DrawAll()
{} 

void SpectrometerSD::PrintAll()
{} 





