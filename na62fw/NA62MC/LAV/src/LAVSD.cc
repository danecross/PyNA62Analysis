// LAVSD.cc
// --------------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch)
// 2009-03-02 - Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - create LAV version of hits
// 2010-03-15 - Domenico Di Filippo (difilippo@na.infn.it)
//   - using OpticalTracker to manage Optical Photons
// 2010-11-23 - Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Changed logic of channel id to take into account LAVBanana
//
// --------------------------------------------------------------------
#include "LAVSD.hh"
#include "LAVHit.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4Track.hh"
#include "G4TrackStatus.hh"
#include "G4Step.hh"
#include "G4ParticleDefinition.hh"
#include "G4VTouchable.hh"
#include "G4TouchableHistory.hh"
#include "G4ios.hh"
#include "G4SDManager.hh"
#include "G4HCofThisEvent.hh"
#include "G4ParticleTypes.hh"
#include "MCTruthTrackInformation.hh"
#include "G4ParticleDefinition.hh"
#include "G4ParticleTable.hh"

using namespace std;

map<int,LAVHit*> LAVSD::fChMap;
LAVHitsCollection* LAVSD::Collection=nullptr;

LAVSD::LAVSD(G4String name,G4String colName) :
  G4VSensitiveDetector(name),
  HCID(0),
  fHierarchy(1)
{
  G4String HCname;
  collectionName.insert(colName);
}

LAVSD::~LAVSD()
{;}

void LAVSD::Initialize(G4HCofThisEvent* HCE)
{
  static int HCID = -1;
  if (Collection != 0) return;
  Collection = new LAVHitsCollection(SensitiveDetectorName,collectionName[0]);
  if (HCID<0) HCID = GetCollectionID(0);
  HCE->AddHitsCollection(HCID,Collection);
}

G4bool LAVSD::ProcessHits(G4Step* aStep,G4TouchableHistory*)
{

  // This is the total energy deposited in this step
  G4double totalEnergyDeposit = aStep->GetTotalEnergyDeposit();

  // Get track which generated this hit
  G4int TrackID = aStep->GetTrack()->GetTrackID();

  // All geometrical and kinematical quantities are taken at the beginning of the step
  G4StepPoint* hitPoint = aStep->GetPreStepPoint();

  // Touchable is needed to convert betweeen global<->local coordinate systems
  G4TouchableHandle theTouchable = hitPoint->GetTouchableHandle(); 

  // Channel id is obtained from the banana id plus the position of the block in the banana
  // Final block id is SSLBBb (SS=station(0-11), L=layer(0-4), BB=banana(0-15), b=block(0-3)
  G4int blockID = theTouchable->GetCopyNumber(fHierarchy);
  G4int bananaID = theTouchable->GetCopyNumber(fHierarchy+1);
  G4int ChannelID = 10*bananaID+blockID;

  // Get position of hit in global and local (within PbGl) coordinates
  G4ThreeVector globalHitPosition = hitPoint->GetPosition();
  G4ThreeVector localHitPosition =
    theTouchable->GetHistory()->GetTopTransform().TransformPoint(globalHitPosition);

  // Get track direction in global and local (within PbGl) coordinates
  G4ThreeVector globalTrajectoryDirection = hitPoint->GetMomentumDirection();
  G4ThreeVector localTrajectoryDirection =
    theTouchable->GetHistory()->GetTopTransform().TransformAxis(globalTrajectoryDirection);

  LAVHit* hit = 0;
  
  /*// Step by step hits
  hit = new LAVHit();
  OpticalTracker.SetEnergyVector(hit->GetPhotonsEnergy());
  OpticalTracker.SetTimeVector(hit->GetPhotonsTime());
  OpticalTracker.ProcessOpticalPhoton(aStep);
  if (totalEnergyDeposit>0 || hit->GetPhotonsEnergy()->size()>0){
     hit->SetChannelID(ChannelID);
     hit->SetEnergy(totalEnergyDeposit);
     hit->SetTime(hitPoint->GetGlobalTime());
     hit->SetTrackID(TrackID);
     hit->SetPosition(globalHitPosition);
     hit->SetLocalPosition(localHitPosition);
     hit->SetLocalDirection(localTrajectoryDirection);
     hit->SetBeta(hitPoint->GetBeta());
     hit->SetStepLength(aStep->GetStepLength());
     Collection->insert(hit);
  }*/

  // Find or create an hit in the [blockID -> hit] table
  map<int,LAVHit*>::iterator hitpair = fChMap.find(ChannelID);
  if ( hitpair == fChMap.end()){

    hit = new LAVHit();
    fChMap[ChannelID] = hit;
    hit->SetChannelID(ChannelID);
    hit->SetEnergy(totalEnergyDeposit);

    hit->SetTime(hitPoint->GetGlobalTime());
    hit->SetTrackID(TrackID);
    hit->SetPosition(globalHitPosition);
    hit->SetLocalPosition(localHitPosition);
    hit->SetLocalDirection(localTrajectoryDirection);
    hit->SetBeta(hitPoint->GetBeta());
    hit->SetStepLength(aStep->GetStepLength());
    
  } else {

    hit = hitpair->second;
    hit->SetEnergy(hit->GetEnergy()+totalEnergyDeposit);
    
    if (hit->GetTime() > hitPoint->GetGlobalTime()){
       hit->SetTime(hitPoint->GetGlobalTime());
       hit->SetTrackID(TrackID);
       hit->SetPosition(globalHitPosition);
       hit->SetLocalPosition(localHitPosition);
       hit->SetLocalDirection(localTrajectoryDirection);
       hit->SetBeta(hitPoint->GetBeta());
       hit->SetStepLength(aStep->GetStepLength());
    }
  }
   
  // Kill photons
  OpticalTracker.SetEnergyVector(hit->GetPhotonsEnergy());
  OpticalTracker.SetTimeVector(hit->GetPhotonsTime());
  OpticalTracker.ProcessOpticalPhoton(aStep);

  if ( verboseLevel> 2 ) hit->Print();

  return true;

}

void LAVSD::EndOfEvent(G4HCofThisEvent*)
{
   if (Collection == 0) return;
   for(map<int,LAVHit*>::iterator i = fChMap.begin(); i != fChMap.end(); ++i)
      Collection->insert(i->second);
   fChMap.clear();
   Collection = 0;
}

void LAVSD::clear()
{} 

void LAVSD::DrawAll()
{
  if (Collection) Collection->DrawAllHits();
} 

void LAVSD::PrintAll()
{
  if (Collection) Collection->PrintAllHits();
} 
