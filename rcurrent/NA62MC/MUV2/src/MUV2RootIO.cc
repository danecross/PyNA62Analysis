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
// Changes to MUV2 by ykohl in March 2010
//
//
// Modified Mario Vormstein (mario.vormstein@cern.ch)  2011-02-01
//
// --------------------------------------------------------------------

#include "MUV2RootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "MUV2GeometryParameters.hh"
#include "MUV2MaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TMUV2Event.hh"
#include "TMUV2Hit.hh"
#include "MUV2SD.hh"
#include "TProcessID.h"

MUV2RootIO::MUV2RootIO() :
  NA62VRootIO(G4String("MUV2")),
  fMUV2Branch(nullptr)
{
  fGeoPars = MUV2GeometryParameters::GetInstance();
  fMatPars = MUV2MaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TMUV2Event();
  fEvent->SetIsMC(kTRUE);

  TTree::SetBranchStyle(fBranchStyle);

  G4cout << "MUV2RootIO: Initialized" << G4endl;
}

MUV2RootIO::~MUV2RootIO() {
  delete fEvent;
}

void MUV2RootIO::Close() {}

void MUV2RootIO::NewRun() {
  // Create a branch to hold MUV2 Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fMUV2Branch = fEventTree->Branch("MUV2", fEvent->IsA()->GetName(), &fEvent);
  fMUV2Branch->SetAutoDelete(kFALSE);
}

void MUV2RootIO::EndRun() {
  if (fVerbose) G4cout << "MUV2RootIO: Executing End-of-Run procedure" << G4endl;
}

void MUV2RootIO::SaveEvent( const G4Event* eventG4 ) {
  if ( fVerbose >= 2 )
    G4cout << "MUV2RootIO: Preparing event structure" << G4endl;

  //Save current Object count
  Int_t savedObjNumber = TProcessID::GetObjectCount();
  G4int nEvent = eventG4->GetEventID();
  fEvent->Clear("C");
  fEvent->SetID(nEvent);
  fEvent->SetRunID(G4RunManager::GetRunManager()->GetCurrentRun()->GetRunID());

  // Get list of hit collections in this event
  G4HCofThisEvent* LHC = eventG4->GetHCofThisEvent();
  G4int nHC = LHC->GetNumberOfCollections();



  for ( G4int iHC=0; iHC<nHC; iHC++) {

    // Handle each collection type with the right method
    G4String HCname = LHC->GetHC(iHC)->GetName();

    if ( fVerbose >= 2 )
      G4cout << "MUV2RootIO: Found hits collection " << HCname << G4endl;
    if ( HCname == "MUV2Collection" ) {

      MUV2HitsCollection* MUV2C = static_cast<MUV2HitsCollection*>( (LHC->GetHC(iHC)));
      if ( MUV2C ) {
        int n_hit = MUV2C->entries();
        if ( n_hit > 0 ) {
          for ( G4int i=0; i<n_hit; i++ ) {
            TMUV2Hit * Hit = static_cast<TMUV2Hit*>(fEvent->AddHit());
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*MUV2C)[i]->GetTrackID()));
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*MUV2C)[i]->GetTrackID()));
            Hit->SetChannelID((*MUV2C)[i]->GetChannelID());
            Hit->SetScintillatorID((*MUV2C)[i]->GetScintillatorID());
            Hit->SetPosition( TVector3( (*MUV2C)[i]->GetPosition()[0],
                  (*MUV2C)[i]->GetPosition()[1],
                  (*MUV2C)[i]->GetPosition()[2] ) );
            Hit->SetTime((*MUV2C)[i]->GetTime());
            Hit->SetEnergy((*MUV2C)[i]->GetEnergy());
            Hit->SetStepLength((*MUV2C)[i]->GetStepLength());
            Hit->SetPositionInScintillator(
                (*MUV2C)[i]->GetPositionInScintillator());
          }
        }
      }
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
