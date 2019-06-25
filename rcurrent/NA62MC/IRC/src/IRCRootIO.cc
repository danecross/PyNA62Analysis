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

#include "IRCRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "IRCGeometryParameters.hh"
#include "IRCMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TIRCEvent.hh"
#include "TIRCHit.hh"
#include "IRCSD.hh"
#include "TProcessID.h"

IRCRootIO::IRCRootIO() :
  NA62VRootIO(G4String("IRC")),
  fIRCBranch(nullptr)
{
  fGeoPars = IRCGeometryParameters::GetInstance();
  fMatPars = IRCMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TIRCEvent();
  fEvent->SetIsMC(kTRUE);

  TTree::SetBranchStyle(fBranchStyle);

  G4cout << "IRCRootIO: Initialized" << G4endl;
}

IRCRootIO::~IRCRootIO() {
  delete fEvent;
}

void IRCRootIO::Close() {}

void IRCRootIO::NewRun() {
  // Create a branch to hold IRC Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fIRCBranch = fEventTree->Branch("IRC", fEvent->IsA()->GetName(), &fEvent);
  fIRCBranch->SetAutoDelete(kFALSE);
}

void IRCRootIO::EndRun() {
  if (fVerbose) G4cout << "IRCRootIO: Executing End-of-Run procedure" << G4endl;
}

void IRCRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "IRCRootIO: Preparing event structure" << G4endl;

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
    if(fVerbose>=2) G4cout << "IRCRootIO: Found hits collection " << HCname << G4endl;
    if (HCname == "IRCCollection"){
      IRCHitsCollection* IRCC = static_cast<IRCHitsCollection*>((LHC->GetHC(iHC)));
      if(IRCC) {
        int n_hit = IRCC->entries();
        if(n_hit>0){
          for(G4int i=0;i<n_hit;i++) {
            TIRCHit * Hit = static_cast<TIRCHit*>(fEvent->AddHit());
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*IRCC)[i]->GetTrackID()));
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*IRCC)[i]->GetTrackID()));
            Hit->SetPosition(TVector3((*IRCC)[i]->GetPosition()[0],
                  (*IRCC)[i]->GetPosition()[1],
                  (*IRCC)[i]->GetPosition()[2])
                );
            Hit->SetChannelID((*IRCC)[i]->GetChannelID());
            Hit->DecodeChannelID();
            Hit->SetTime((*IRCC)[i]->GetTime());
            Hit->SetEnergy((*IRCC)[i]->GetEnergy());
          }
        }
      }
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
