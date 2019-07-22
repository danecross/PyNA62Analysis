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
//            Evelina Marinova (Evelina.Marinova@cern.ch)
//
// Modified by Sergey Podolsky (siarhei.padolski@cern.ch) 2012-09-14
// --------------------------------------------------------------------

#include "LKrRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "LKrGeometryParameters.hh"
#include "LKrMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TLKrEvent.hh"
#include "TLKrHit.hh"
#include "LKrSD.hh"
#include "TProcessID.h"

LKrRootIO::LKrRootIO() :
  NA62VRootIO(G4String("LKr")),
  fLKrBranch(nullptr)
{

  fGeoPars = LKrGeometryParameters::GetInstance();
  fMatPars = LKrMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TLKrEvent();

  TTree::SetBranchStyle(fBranchStyle);

  G4cout << "LKrRootIO: Initialized" << G4endl;
}

LKrRootIO::~LKrRootIO() {
  delete fEvent;
}

void LKrRootIO::Close() {}

void LKrRootIO::NewRun() {
  // Create branch to hold LKr Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fLKrBranch = fEventTree->Branch("LKr", fEvent->IsA()->GetName(), &fEvent);
  fLKrBranch->SetAutoDelete(kFALSE);
}

void LKrRootIO::EndRun() {
  if (fVerbose) G4cout << "LKrRootIO: Executing End-of-Run procedure" << G4endl;
}

void LKrRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "LKrRootIO: Preparing event structure" << G4endl;

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
    if(fVerbose>=2) G4cout << "LKrRootIO: Found hits collection " << HCname << G4endl;
    if (HCname == "LKrCollection"){
      LKrHitsCollection* LKrC = static_cast<LKrHitsCollection*>((LHC->GetHC(iHC)));
      if(LKrC) {
        int n_hit = LKrC->entries();
        if(n_hit>0){
          for(G4int i=0;i<n_hit;i++) {
            //if((*LKrC)[i]->GetEnergy() < 3*MeV) continue;
            TLKrHit * Hit = static_cast<TLKrHit*>(fEvent->AddHit());
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*LKrC)[i]->GetTrackID()));
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*LKrC)[i]->GetTrackID()));
            Hit->SetChannelID((*LKrC)[i]->GetChannelID());
            Hit->DecodeChannelID();
            Hit->SetPosition(TVector3((*LKrC)[i]->GetPosition()[0],
                  (*LKrC)[i]->GetPosition()[1],
                  (*LKrC)[i]->GetPosition()[2])
                );
            Hit->SetTime((*LKrC)[i]->GetTime());
            Hit->SetEnergy((*LKrC)[i]->GetEnergy());
            ////<<                        Hit->SetMicroCellData((*LKrC)[i]->GetMicroCellData());
            Hit->SetCurrent((*LKrC)[i]->GetCurrent());

            //Hit->SetLongitudinalDevelopment((*LKrC)[i]->GetLongitudinalDevelopment());

            //  if((*LKrC)[i]->GetEnergy()> 100 *MeV)
            //  fEvent->AddSeed(i);
          }
        }
      }
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
