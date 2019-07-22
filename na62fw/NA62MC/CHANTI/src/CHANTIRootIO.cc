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
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//
// --------------------------------------------------------------------

#include "CHANTIRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "CHANTIGeometryParameters.hh"
#include "CHANTIMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TCHANTIEvent.hh"
#include "TCHANTIHit.hh"
#include "CHANTISD.hh"
#include "TProcessID.h"

CHANTIRootIO::CHANTIRootIO() :
  NA62VRootIO(G4String("CHANTI")),
  fCHANTIBranch(nullptr)
{

  fGeoPars = CHANTIGeometryParameters::GetInstance();
  fMatPars = CHANTIMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TCHANTIEvent();

  TTree::SetBranchStyle(fBranchStyle);

  G4cout << "CHANTIRootIO: Initialized" << G4endl;

}

CHANTIRootIO::~CHANTIRootIO() {
  delete fEvent;
}

void CHANTIRootIO::Close() {}

void CHANTIRootIO::NewRun() {
  // Create a branch to hold CHANTI Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fCHANTIBranch = fEventTree->Branch("CHANTI", fEvent->IsA()->GetName(), &fEvent);
  fCHANTIBranch->SetAutoDelete(kFALSE);
}

void CHANTIRootIO::EndRun() {
  if (fVerbose) G4cout << "CHANTIRootIO: Executing End-of-Run procedure" << G4endl;
}

void CHANTIRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "CHANTIRootIO: Preparing event structure" << G4endl;

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
    if(fVerbose>=2) G4cout << "CHANTIRootIO: Found hits collection " << HCname << G4endl;
    if (HCname == "CHANTICollection"){
      CHANTIHitsCollection* CHANTIC = (CHANTIHitsCollection*)(LHC->GetHC(iHC));
      if(CHANTIC) {
        int n_hit = CHANTIC->entries();
        if(n_hit>0){
          for(G4int i=0;i<n_hit;i++) {
            TCHANTIHit * Hit = static_cast<TCHANTIHit*>(fEvent->AddHit());
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*CHANTIC)[i]->GetTrackID()));
            Hit->SetChannelID((*CHANTIC)[i]->GetChannelID());
            Hit->DecodeChannelID();
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*CHANTIC)[i]->GetTrackID()));
            Hit->SetPosition(TVector3((*CHANTIC)[i]->GetPosition()[0],
                      (*CHANTIC)[i]->GetPosition()[1],
                      (*CHANTIC)[i]->GetPosition()[2])
            );
            Hit->SetTime((*CHANTIC)[i]->GetTime());
            Hit->SetEnergy((*CHANTIC)[i]->GetEnergy());
          }
        }
      }
    }
  }

  TProcessID::SetObjectCount(savedObjNumber);
}
