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
//
// --------------------------------------------------------------------

#include "MUV3RootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TMUV3Event.hh"
#include "TMUV3Hit.hh"
#include "MUV3SD.hh"
#include "TProcessID.h"

MUV3RootIO::MUV3RootIO() :
  NA62VRootIO(G4String("MUV3")),
  fMUV3Branch(nullptr)
{
  // Create run and event objects
  fEvent = new TMUV3Event();
  fEvent->SetIsMC(kTRUE);
  TTree::SetBranchStyle(fBranchStyle);
  G4cout << "MUV3RootIO: Initialized" << G4endl;
}

MUV3RootIO::~MUV3RootIO() {
  delete fEvent;
}

void MUV3RootIO::NewRun() {
  // Create a branch to hold MUV3 Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fMUV3Branch = fEventTree->Branch("MUV3", fEvent->IsA()->GetName(), &fEvent);
  fMUV3Branch->SetAutoDelete(kFALSE);
}

void MUV3RootIO::EndRun() {
  if (fVerbose) G4cout << "MUV3RootIO: Executing End-of-Run procedure" << G4endl;
}

void MUV3RootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "MUV3RootIO: Preparing event structure" << G4endl;

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
    if (fVerbose>=2) G4cout << "MUV3RootIO: Found hits collection " << HCname << G4endl;

    if (HCname != "MUV3Collection") continue;
    MUV3HitsCollection* MUV3C = static_cast<MUV3HitsCollection*>((LHC->GetHC(iHC)));
    if (!MUV3C) continue;

    for (G4int i=0; i<MUV3C->entries(); i++) {
      TMUV3Hit* Hit = static_cast<TMUV3Hit*>(fEvent->AddHit());
      Hit->SetMCTrackID
        (MCTruthManager::GetInstance()->FindKinePartIndex((*MUV3C)[i]->GetTrackID()));
      Hit->SetDirectInteraction
        (MCTruthManager::GetInstance()->DirectInteraction((*MUV3C)[i]->GetTrackID()));
      Hit->SetChannelID((*MUV3C)[i]->GetChannelID());
      Hit->DecodeChannelID();
      Hit->SetPosition(TVector3((*MUV3C)[i]->GetPosition()[0],
            (*MUV3C)[i]->GetPosition()[1],
            (*MUV3C)[i]->GetPosition()[2]));
      Hit->SetTime((*MUV3C)[i]->GetTime());
      Hit->SetEnergy((*MUV3C)[i]->GetEnergy());
      Hit->SetMuonHit((*MUV3C)[i]->GetMuonHit());
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
