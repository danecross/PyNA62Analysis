#include "RICHPMTSD.hh"
#include "RICHPMTHit.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4Track.hh"
#include "G4Step.hh"
#include "G4ParticleDefinition.hh"
#include "G4VTouchable.hh"
#include "G4VProcess.hh"
#include "G4TouchableHistory.hh"
#include "G4ios.hh"
#include "G4SDManager.hh"
#include "G4HCofThisEvent.hh"
#include "G4ParticleTypes.hh"
#include "MCTruthTrackInformation.hh"

RICHPMTSD::RICHPMTSD(G4String name,G4String colName) :
  G4VSensitiveDetector(name),
  Collection(nullptr),
  nHits(0),
  HCID(0)
{
  G4String HCname;
  collectionName.insert(HCname=colName);
}

RICHPMTSD::~RICHPMTSD()
{;}

void RICHPMTSD::Initialize(G4HCofThisEvent*HCE)
{
  static int HCID = -1;
  Collection = new RICHPMTHitsCollection(SensitiveDetectorName,collectionName[0]);
  verboseLevel = 0;
  nHits=0;
  if(HCID<0)
    { HCID = GetCollectionID(0); }
  HCE->AddHitsCollection( HCID, Collection );
}

G4bool RICHPMTSD::ProcessHits(G4Step*aStep,G4TouchableHistory*)
{

  G4double edep = aStep->GetTotalEnergyDeposit();
  G4ParticleDefinition * particle = aStep->GetTrack()->GetDefinition();
  //need to know if this is an optical photon
  if(particle != G4OpticalPhoton::OpticalPhotonDefinition() 
     && particle->GetPDGCharge() == 0){
 return false;
}


//  if( edep == 0 ) return false;

  G4int PositionID;  
  G4double XPosition=aStep->GetPostStepPoint()->GetPosition()[0];
 
  G4StepPoint* preStepPoint = aStep->GetPreStepPoint();
  G4TouchableHandle theTouchable = preStepPoint->GetTouchableHandle();
  //G4int copyNo = theTouchable->GetCopyNumber();
  G4int motherCopyNo = theTouchable->GetCopyNumber(1);

  if(XPosition<0){
    PositionID= motherCopyNo+976;
  }else{
    PositionID= motherCopyNo;
  }

  //  G4cout<<"PositionID: "<<PositionID<<G4endl;

  RICHPMTHit* pmtHit = new RICHPMTHit();
  pmtHit->SetPosition( aStep->GetPostStepPoint()->GetPosition() );
  pmtHit->SetTime(aStep->GetPostStepPoint()->GetGlobalTime());
  pmtHit->SetPositionID(PositionID);

  if(particle == G4OpticalPhoton::OpticalPhotonDefinition())
    pmtHit->SetEnergy(aStep->GetTrack()->GetTotalEnergy());
  else
    pmtHit->SetEnergy(edep);

  pmtHit->SetTrackID(aStep->GetTrack()->GetParentID()); 

  //G4int TrackID = ((MCTruthTrackInformation*)aStep->GetTrack()->GetUserInformation())->GetSavedParentID();
  G4int TrackID = 0;
  if(particle == G4OpticalPhoton::OpticalPhotonDefinition()){
      pmtHit->SetEnergy(aStep->GetTrack()->GetTotalEnergy());
      TrackID = aStep->GetTrack()->GetParentID();
  }else{
      pmtHit->SetEnergy(edep);
      TrackID = aStep->GetTrack()->GetTrackID();
  }
  pmtHit->SetTrackID(TrackID);

  Collection->insert( pmtHit );
  nHits++;
  return true;
}

void RICHPMTSD::EndOfEvent(G4HCofThisEvent*/*HCE*/)
{}

void RICHPMTSD::clear()
{} 


void RICHPMTSD::DrawAll()
{} 

void RICHPMTSD::PrintAll()
{} 





