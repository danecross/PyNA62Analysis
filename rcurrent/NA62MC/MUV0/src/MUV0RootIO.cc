// --------------------------------------------------------------------
// History:
//
// Created by Giuseppe Ruggiero 04-09-2012
// Updated: Evgueni Goudzovski  11-05-2016
//
// --------------------------------------------------------------------

#include "MUV0RootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TMUV0Event.hh"
#include "TMUV0Hit.hh"
#include "MUV0SD.hh"
#include "TProcessID.h"

MUV0RootIO::MUV0RootIO() :
  NA62VRootIO(G4String("MUV0")),
  fMUV0Branch(nullptr)
{
  // Create run and event objects
  fEvent = new TMUV0Event();
  fEvent->SetIsMC(kTRUE);
  TTree::SetBranchStyle(fBranchStyle);
  G4cout << "MUV0RootIO: Initialized" << G4endl;
}

MUV0RootIO::~MUV0RootIO() {
  delete fEvent;
}

void MUV0RootIO::NewRun() {
  // Create a branch to hold MUV0 Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fMUV0Branch = fEventTree->Branch("MUV0", fEvent->IsA()->GetName(), &fEvent);
  fMUV0Branch->SetAutoDelete(kFALSE);
}

void MUV0RootIO::EndRun() {
  if (fVerbose) G4cout << "MUV0RootIO: Executing End-of-Run procedure" << G4endl;
}

void MUV0RootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "MUV0RootIO: Preparing event structure" << G4endl;

  // Save current Object count
  Int_t savedObjNumber = TProcessID::GetObjectCount();
  G4int nEvent = eventG4->GetEventID();
  fEvent->Clear("C");
  fEvent->SetID(nEvent);
  fEvent->SetRunID(G4RunManager::GetRunManager()->GetCurrentRun()->GetRunID());

  // Get list of hit collections in this event
  G4HCofThisEvent* LHC = eventG4->GetHCofThisEvent();
  G4int nHC = LHC->GetNumberOfCollections();

  for (G4int iHC=0; iHC<nHC; iHC++) {

    // Handle each collection type with the right method
    G4String HCname = LHC->GetHC(iHC)->GetName();
    if (fVerbose>=2) G4cout << "MUV0RootIO: Found hits collection " << HCname << G4endl;

    if (HCname != "MUV0Collection") continue;
    MUV0HitsCollection* MUV0C = static_cast<MUV0HitsCollection*>((LHC->GetHC(iHC)));
    if (!MUV0C) continue;

    for (G4int i=0; i<MUV0C->entries(); i++) {
      TMUV0Hit* Hit = static_cast<TMUV0Hit*>(fEvent->AddHit());
      Hit->SetMCTrackID
	(MCTruthManager::GetInstance()->FindKinePartIndex((*MUV0C)[i]->GetTrackID()));
      Hit->SetDirectInteraction
	(MCTruthManager::GetInstance()->DirectInteraction((*MUV0C)[i]->GetTrackID()));
      Hit->SetChannelID((*MUV0C)[i]->GetChannelID());
      Hit->DecodeChannelID();
      Hit->SetPosition(TVector3((*MUV0C)[i]->GetPosition()[0],
				(*MUV0C)[i]->GetPosition()[1],
				(*MUV0C)[i]->GetPosition()[2]));
      Hit->SetTime((*MUV0C)[i]->GetTime());
      Hit->SetEnergy((*MUV0C)[i]->GetEnergy());
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
