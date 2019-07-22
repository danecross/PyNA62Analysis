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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)
//
// --------------------------------------------------------------------

#include "SACRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "SACGeometryParameters.hh"
#include "SACMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TSACEvent.hh"
#include "TSACHit.hh"
#include "SACSD.hh"
#include "TProcessID.h"

SACRootIO::SACRootIO() :
  NA62VRootIO(G4String("SAC")),
  fSACBranch(nullptr)
{

  fGeoPars = SACGeometryParameters::GetInstance();
  fMatPars = SACMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TSACEvent();
  fEvent->SetIsMC(kTRUE);

  TTree::SetBranchStyle(fBranchStyle);
  G4cout << "SACRootIO: Initialized" << G4endl;
}

SACRootIO::~SACRootIO() {
  delete fEvent;
}

void SACRootIO::Close() {}

void SACRootIO::NewRun() {
  // Create a branch to hold SAC Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fSACBranch = fEventTree->Branch("SAC", fEvent->IsA()->GetName(), &fEvent);
  fSACBranch->SetAutoDelete(kFALSE);
}

void SACRootIO::EndRun() {
  if (fVerbose) G4cout << "SACRootIO: Executing End-of-Run procedure" << G4endl;
}

void SACRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "SACRootIO: Preparing event structure" << G4endl;

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
    if(fVerbose>=2) G4cout << "SACRootIO: Found hits collection " << HCname << G4endl;
    if (HCname == "SACCollection"){
      SACHitsCollection* SACC = static_cast<SACHitsCollection*>((LHC->GetHC(iHC)));
      if(SACC) {
        int n_hit = SACC->entries();
        if(n_hit>0){
          for(G4int i=0;i<n_hit;i++) {
            TSACHit * Hit = static_cast<TSACHit*>(fEvent->AddHit());
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*SACC)[i]->GetTrackID()));
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*SACC)[i]->GetTrackID()));
            Hit->SetChannelID((*SACC)[i]->GetChannelID());
            Hit->DecodeChannelID();
            Hit->SetPosition(TVector3((*SACC)[i]->GetPosition()[0],
                  (*SACC)[i]->GetPosition()[1],
                  (*SACC)[i]->GetPosition()[2])
                );
            Hit->SetTime((*SACC)[i]->GetTime());
            Hit->SetEnergy((*SACC)[i]->GetEnergy());
          }
        }
      }
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
