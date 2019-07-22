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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-12
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//
// Changes to MUV1 by ykohl in March 2010
//
// --------------------------------------------------------------------

#include "MUV1RootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "MUV1GeometryParameters.hh"
#include "MUV1MaterialParameters.hh"
#include "MUV1DetectorMessenger.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TMUV1Event.hh"
#include "TMUV1Hit.hh"
#include "MUV1SD.hh"
#include "MUV1PMTSD.hh"
#include "TProcessID.h"

MUV1RootIO::MUV1RootIO() :
  NA62VRootIO(G4String("MUV1")),
  fMUV1Branch(nullptr)
{

    fGeoPars = MUV1GeometryParameters::GetInstance();
    fMatPars = MUV1MaterialParameters::GetInstance();

    // Create run and event objects
    fEvent = new TMUV1Event();
    fEvent->SetIsMC(kTRUE);

    TTree::SetBranchStyle(fBranchStyle);

    G4cout << "MUV1RootIO: Initialized" << G4endl;
  }

MUV1RootIO::~MUV1RootIO() {
    delete fEvent;
}

void MUV1RootIO::Close() {}

void MUV1RootIO::NewRun() {
  // Create a branch to hold MUV1 Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fMUV1Branch = fEventTree->Branch("MUV1", fEvent->IsA()->GetName(), &fEvent);
  fMUV1Branch->SetAutoDelete(kFALSE);
}

void MUV1RootIO::EndRun() {
  if (fVerbose) G4cout << "MUV1RootIO: Executing End-of-Run procedure" << G4endl;
}

void MUV1RootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose >= 2)
    G4cout << "MUV1RootIO: Preparing event structure" << G4endl;

  //Save current Object count
  Int_t savedObjNumber = TProcessID::GetObjectCount();
  G4int nEvent = eventG4->GetEventID();
  fEvent->Clear("C");
  fEvent->SetID(nEvent);
  fEvent->SetRunID(G4RunManager::GetRunManager()->GetCurrentRun()->GetRunID());

  // Get list of hit collections in this event
  G4HCofThisEvent* LHC = eventG4->GetHCofThisEvent();
  G4int nHC = LHC->GetNumberOfCollections();

  for (G4int iHC = 0; iHC < nHC; iHC++) {

    // Handle each collection type with the right method
    G4String HCname = LHC->GetHC(iHC)->GetName();

    if (fVerbose >= 2)
      G4cout << "MUV1RootIO: Found hits collection " << HCname << G4endl;
    if (HCname == "MUV1Collection") {

      MUV1HitsCollection* MUV1C = static_cast<MUV1HitsCollection*>( (LHC->GetHC(iHC)));
      if (MUV1C) {
        int n_hit = MUV1C->entries();
        if (n_hit > 0) {
          for (G4int i = 0; i < n_hit; i++) {
            TMUV1Hit * Hit = static_cast<TMUV1Hit*>( fEvent->AddHit());
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*MUV1C)[i]->GetTrackID()));
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*MUV1C)[i]->GetTrackID()));
            Hit->SetChannelID((*MUV1C)[i]->GetChannelID());
            Hit->SetScintillatorID((*MUV1C)[i]->GetScintillatorID());
            Hit->SetPosition(TVector3(
                  (*MUV1C)[i]->GetPosition()[0],
                  (*MUV1C)[i]->GetPosition()[1],
                  (*MUV1C)[i]->GetPosition()[2]));
            Hit->SetPositionInScintillator(
                (*MUV1C)[i]->GetPositionInScintillator());
            Hit->SetTime((*MUV1C)[i]->GetTime());
            Hit->SetEnergy((*MUV1C)[i]->GetEnergy());
            //Hit->SetOriginalEnergy((*MUV1C)[i]->GetOriginalEnergy());
            Hit->SetStepLength((*MUV1C)[i]->GetStepLength());
            //Hit->SetPhotons((*MUV1C)[i]->GetPhotons());
            //Hit->SetPhotonsLeft((*MUV1C)[i]->GetPhotonsLeft());
            //Hit->SetPhotonsRight((*MUV1C)[i]->GetPhotonsRight());
            //Hit->SetPMTIDLeft((*MUV1C)[i]->GetPMTIDLeft());
            //Hit->SetPMTIDRight((*MUV1C)[i]->GetPMTIDRight());
          }
        }
      }
    }

    if (HCname == "MUV1PMTCollection") {
      MUV1HitsCollection* MUV1C = static_cast<MUV1HitsCollection*>( (LHC->GetHC(iHC)));
      if (MUV1C) {
        int n_hit = MUV1C->entries();
        if (n_hit > 0) {
          for (G4int i = 0; i < n_hit; i++) {
            TMUV1Hit * Hit = static_cast<TMUV1Hit*>( fEvent->AddHit());
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*MUV1C)[i]->GetTrackID()));
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*MUV1C)[i]->GetTrackID()));
            //Hit->SetChannelID((*MUV1C)[i]->GetChannelID());
            //Hit->SetPosition( TVector3( (*MUV1C)[i]->GetPosition()[0],
            //			    (*MUV1C)[i]->GetPosition()[1],
            //			    (*MUV1C)[i]->GetPosition()[2] ) );
            Hit->SetTime((*MUV1C)[i]->GetTime());
            //Hit->SetEnergy((*MUV1C)[i]->GetEnergy());
            Hit->SetPhotons((*MUV1C)[i]->GetPhotons());
            //Hit->SetPhotonsLeft((*MUV1C)[i]->GetPhotonsLeft());
            //Hit->SetPhotonsRight((*MUV1C)[i]->GetPhotonsRight());
            //Hit->SetPMTIDLeft((*MUV1C)[i]->GetPMTIDLeft());
            //Hit->SetPMTIDRight((*MUV1C)[i]->GetPMTIDRight());
          }
        }
      }
    }

  }

  TProcessID::SetObjectCount(savedObjNumber);
}
