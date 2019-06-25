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
// --------------------------------------------------------------
// History:
//
// 2000-03-10 S.Bifani
// - Changed Root TTree splitlevel (3 -> 5)
//
// 2008-04-29 S.Bifani
// - Changed Root TTree information (hit -> event)
//
// 2008-04-22 S.Bifani (Simone.Bifani@cern.ch)
// - Added main GTK info to the Root TTree (pixel & station ID,
//   energy & time, position, simulated track ID)
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// --------------------------------------------------------------
//
#include <sstream>
#include "GigaTrackerRootIO.hh"
#include "RootIOManager.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerMaterialParameters.hh"
#include "GigaTrackerSD.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "G4Trajectory.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TGigaTrackerEvent.hh"
#include "TGigaTrackerHit.hh"
#include "TProcessID.h"

GigaTrackerRootIO::GigaTrackerRootIO() :
  NA62VRootIO(G4String("GigaTracker")),
  fGigaTrackerBranch(nullptr)
{
    fGeoPars = GigaTrackerGeometryParameters::GetInstance();
    fMatPars = GigaTrackerMaterialParameters::GetInstance();

    // Create run and event objects
    fEvent = new TGigaTrackerEvent();
    fEvent->SetIsMC(kTRUE);

    TTree::SetBranchStyle(fBranchStyle);

    G4cout << "GigaTrackerRootIO: Initialized" << G4endl;

}

GigaTrackerRootIO::~GigaTrackerRootIO() {
    delete fEvent;
}

void GigaTrackerRootIO::Close() {}

void GigaTrackerRootIO::NewRun() {
  // Create a branch to hold GigaTracker Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fGigaTrackerBranch = fEventTree->Branch("GigaTracker", fEvent->IsA()->GetName(), &fEvent);
  fGigaTrackerBranch->SetAutoDelete(kFALSE);
}

void GigaTrackerRootIO::EndRun() {
  if (fVerbose) G4cout << "GigaTrackerRootIO: Executing End-of-Run procedure" << G4endl;
}

void GigaTrackerRootIO::SaveEvent(const G4Event* eventG4) {

  if (fVerbose>=2) G4cout << "GigaTrackerRootIO: Preparing event structure" << G4endl;

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
    if(fVerbose>=2) G4cout << "GigaTrackerRootIO: Found hits collection " << HCname << G4endl;
    if (HCname == "GigaTrackerCollection"){
      GigaTrackerHitsCollection* GigaTrackerC = static_cast<GigaTrackerHitsCollection*>((LHC->GetHC(iHC)));
      if(GigaTrackerC) {
          int n_hit = GigaTrackerC->entries();

          if(n_hit>0){
              for(G4int i=0;i<n_hit;i++) {
                  TGigaTrackerHit* Hit;
                  Hit = static_cast<TGigaTrackerHit*>(fEvent->AddHit());
                  Hit->SetStationNo((*GigaTrackerC)[i]->GetStationNo());
                  Hit->SetPixelID((*GigaTrackerC)[i]->GetPixelID());
                  Hit->EncodeChannelID();
                  Hit->SetTime((*GigaTrackerC)[i]->GetTime());
                  Hit->SetEnergy((*GigaTrackerC)[i]->GetEnergy());

                  Hit->SetPosition(TVector3((*GigaTrackerC)[i]->GetPosition()[0],
                          (*GigaTrackerC)[i]->GetPosition()[1],
                          (*GigaTrackerC)[i]->GetPosition()[2])
                  );
                  Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*GigaTrackerC)[i]->GetTrackID()));
                  Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*GigaTrackerC)[i]->GetTrackID()));

              }
          }
      }
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
