// --------------------------------------------------------------------
// History:
//
// Created by Giuseppe Ruggiero 04-09-2012
//
// --------------------------------------------------------------------

#include "HACRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "HACGeometryParameters.hh"
#include "HACMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "THACEvent.hh"
#include "THACHit.hh"
#include "HACSD.hh"
#include "TProcessID.h"

HACRootIO::HACRootIO() :
  NA62VRootIO(G4String("HAC")),
  fHACBranch(nullptr)
{

  fGeoPars = HACGeometryParameters::GetInstance();
  fMatPars = HACMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new THACEvent();
  fEvent->SetIsMC(kTRUE);

  TTree::SetBranchStyle(fBranchStyle);
  G4cout << "HACRootIO: Initialized" << G4endl;
}

HACRootIO::~HACRootIO() {
  delete fEvent;
}

void HACRootIO::Close() {}

void HACRootIO::NewRun() {
  // Create a branch to hold HAC Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fHACBranch = fEventTree->Branch("HAC", fEvent->IsA()->GetName(), &fEvent);
  fHACBranch->SetAutoDelete(kFALSE);
}

void HACRootIO::EndRun() {
  if (fVerbose) G4cout << "HACRootIO: Executing End-of-Run procedure" << G4endl;
}

void HACRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "HACRootIO: Preparing event structure" << G4endl;

  //Save current Object count
  Int_t savedObjNumber = TProcessID::GetObjectCount();
  G4int nEvent = eventG4->GetEventID();
  fEvent->Clear("C");
  fEvent->SetID(nEvent);
  fEvent->SetRunID(G4RunManager::GetRunManager()->GetCurrentRun()->GetRunID());

  // Get list of hit collections in this event
  G4HCofThisEvent* LHC = eventG4->GetHCofThisEvent();
  G4int nHC = LHC->GetNumberOfCollections();

  for(G4int iHC=0; iHC<nHC; iHC++) {

    // Handle each collection type with the right method
    G4String HCname = LHC->GetHC(iHC)->GetName();
    if(fVerbose>=2) G4cout << "HACRootIO: Found hits collection " << HCname << G4endl;
    if (HCname == "HACCollection"){
      HACHitsCollection* HACC = static_cast<HACHitsCollection*>((LHC->GetHC(iHC)));
      if(HACC) {
        int n_hit = HACC->entries();
        if(n_hit>0){
          for(G4int i=0;i<n_hit;i++) {
            THACHit * Hit = static_cast<THACHit*>(fEvent->AddHit());
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*HACC)[i]->GetTrackID()));
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*HACC)[i]->GetTrackID()));
            Hit->SetChannelID((*HACC)[i]->GetChannelID());
            Hit->SetPosition(TVector3((*HACC)[i]->GetPosition()[0],(*HACC)[i]->GetPosition()[1],(*HACC)[i]->GetPosition()[2]));
            Hit->SetTime((*HACC)[i]->GetTime());
            Hit->SetEnergy((*HACC)[i]->GetEnergy());
          }
        }
      }
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
