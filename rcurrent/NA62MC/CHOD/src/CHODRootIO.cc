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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2008-02-03
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//
// --------------------------------------------------------------------

#include "CHODRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "CHODGeometryParameters.hh"
#include "CHODMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TCHODEvent.hh"
#include "TCHODHit.hh"
#include "CHODSD.hh"
#include "TProcessID.h"

CHODRootIO::CHODRootIO() :
  NA62VRootIO(G4String("CHOD")),
  fCHODBranch(nullptr)
{

  fGeoPars = CHODGeometryParameters::GetInstance();
  fMatPars = CHODMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TCHODEvent();
  fEvent->SetIsMC(kTRUE);
  TTree::SetBranchStyle(fBranchStyle);
  G4cout << "CHODRootIO: Initialized" << G4endl;
}

CHODRootIO::~CHODRootIO() {
  delete fEvent;
}

void CHODRootIO::NewRun() {
  // Create a branch to hold CHOD Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fCHODBranch = fEventTree->Branch("CHOD", fEvent->IsA()->GetName(), &fEvent);
  fCHODBranch->SetAutoDelete(kFALSE);
}

void CHODRootIO::EndRun() {
  if (fVerbose) G4cout << "CHODRootIO: Executing End-of-Run procedure" << G4endl;
}

void CHODRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "CHODRootIO: Preparing event structure" << G4endl;

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
    if(fVerbose>=2) G4cout << "CHODRootIO: Found hits collection " << HCname << G4endl;

    if (HCname != "CHODCollection") continue;
    CHODHitsCollection* CHODC = static_cast<CHODHitsCollection*>((LHC->GetHC(iHC)));
    if (!CHODC) continue;

    for (G4int i=0; i<CHODC->entries(); i++) {
      TCHODHit * Hit = static_cast<TCHODHit*>(fEvent->AddHit());
      Hit->SetMCTrackID
	(MCTruthManager::GetInstance()->FindKinePartIndex((*CHODC)[i]->GetTrackID()));
      Hit->SetDirectInteraction
	(MCTruthManager::GetInstance()->DirectInteraction((*CHODC)[i]->GetTrackID()));
      Hit->SetChannelID((*CHODC)[i]->GetChannelID());
      Hit->DecodeChannelID();
      Hit->SetPosition(TVector3((*CHODC)[i]->GetPosition()[0],
				(*CHODC)[i]->GetPosition()[1],
				(*CHODC)[i]->GetPosition()[2]));
      Hit->SetTime((*CHODC)[i]->GetTime());
      Hit->SetEnergy((*CHODC)[i]->GetEnergy());
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
