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

#include "NewCHODRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "NewCHODGeometryParameters.hh"
#include "NewCHODMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TNewCHODEvent.hh"
#include "TNewCHODHit.hh"
#include "NewCHODSD.hh"
#include "TProcessID.h"

NewCHODRootIO::NewCHODRootIO() :
  NA62VRootIO(G4String("NewCHOD")),
  fNewCHODBranch(nullptr)
{

  fGeoPars = NewCHODGeometryParameters::GetInstance();
  fMatPars = NewCHODMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TNewCHODEvent();
  fEvent->SetIsMC(kTRUE);
  TTree::SetBranchStyle(fBranchStyle);
  G4cout << "NewCHODRootIO: Initialized" << G4endl;
}

NewCHODRootIO::~NewCHODRootIO() {
  delete fEvent;
}

void NewCHODRootIO::NewRun() {
  // Create a branch to hold NewCHOD Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fNewCHODBranch = fEventTree->Branch("NewCHOD", fEvent->IsA()->GetName(), &fEvent);
  fNewCHODBranch->SetAutoDelete(kFALSE);
}

void NewCHODRootIO::EndRun() {
  if (fVerbose) G4cout << "NewCHODRootIO: Executing End-of-Run procedure" << G4endl;
}

void NewCHODRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "NewCHODRootIO: Preparing event structure" << G4endl;

  // Save current object count
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
    if(fVerbose>=2) G4cout << "NewCHODRootIO: Found hits collection " << HCname << G4endl;

    if (HCname != "NewCHODCollection") continue;
    NewCHODHitsCollection* NewCHODC = static_cast<NewCHODHitsCollection*>((LHC->GetHC(iHC)));
    if (!NewCHODC) continue;

    for (G4int i=0; i<NewCHODC->entries(); i++) {
      TNewCHODHit *Hit = static_cast<TNewCHODHit*>(fEvent->AddHit());
      Hit->SetMCTrackID
	(MCTruthManager::GetInstance()->FindKinePartIndex((*NewCHODC)[i]->GetTrackID()));
      Hit->SetDirectInteraction
	(MCTruthManager::GetInstance()->DirectInteraction((*NewCHODC)[i]->GetTrackID()));
      Hit->SetChannelID((*NewCHODC)[i]->GetChannelID());
      Hit->DecodeChannelID();
      Hit->SetPosition(TVector3((*NewCHODC)[i]->GetPosition()[0],
				(*NewCHODC)[i]->GetPosition()[1],
				(*NewCHODC)[i]->GetPosition()[2]));
      Hit->SetTime((*NewCHODC)[i]->GetTime());
      Hit->SetEnergy((*NewCHODC)[i]->GetEnergy());
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
