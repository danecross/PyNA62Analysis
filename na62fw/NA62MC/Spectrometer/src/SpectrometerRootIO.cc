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
// 2008-04-23 A.Sergi   Changed TTree content to TSpectrometerEvent
//                      Multiple hits in single Straw Tube are summed
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------------

#include "SpectrometerRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "SpectrometerGeometryParameters.hh"
#include "SpectrometerMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TSpectrometerEvent.hh"
#include "TSpectrometerHit.hh"
#include "SpectrometerSD.hh"
#include "TProcessID.h"

SpectrometerRootIO::SpectrometerRootIO() :
  NA62VRootIO(G4String("Spectrometer")),
  fSpectrometerBranch(nullptr)
{

  fGeoPars = SpectrometerGeometryParameters::GetInstance();
  fMatPars = SpectrometerMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TSpectrometerEvent();
  fEvent->SetIsMC(kTRUE);

  TTree::SetBranchStyle(fBranchStyle);

  G4cout << "SpectrometerRootIO: Initialized" << G4endl;

}

SpectrometerRootIO::~SpectrometerRootIO() {
  delete fEvent;
}

void SpectrometerRootIO::Close() {}

void SpectrometerRootIO::NewRun() {
  // Create a branch to hold Spectrometer Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fSpectrometerBranch = fEventTree->Branch("Spectrometer", fEvent->IsA()->GetName(), &fEvent);
  fSpectrometerBranch->SetAutoDelete(kFALSE);
}

void SpectrometerRootIO::EndRun() {
  if (fVerbose) G4cout << "SpectrometerRootIO: Executing End-of-Run procedure" << G4endl;
}

void SpectrometerRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "SpectrometerRootIO: Preparing event structure" << G4endl;

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
    if(fVerbose>=2) G4cout << "SpectrometerRootIO: Found hits collection " << HCname << G4endl;
    if (HCname == "SpectrometerCollection"){
      SpectrometerHitsCollection* SpectrometerC = static_cast<SpectrometerHitsCollection*>((LHC->GetHC(iHC)));
      if(SpectrometerC) {
        int n_hit = SpectrometerC->entries();
        if(n_hit>0){
          for(G4int i=0;i<n_hit;i++) {
            TSpectrometerHit* Hit = static_cast<TSpectrometerHit*>(fEvent->AddHit());
            Hit->SetStrawID((*SpectrometerC)[i]->GetStrawID()%1000);
            Hit->SetPlaneID((*SpectrometerC)[i]->GetStrawID()/1000);
            Hit->SetHalfViewID((*SpectrometerC)[i]->GetHalfViewID());
            Hit->SetViewID((*SpectrometerC)[i]->GetViewID());
            Hit->SetChamberID((*SpectrometerC)[i]->GetChamberID());
            Hit->EncodeChannelID();
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*SpectrometerC)[i]->GetTrackID()));
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*SpectrometerC)[i]->GetTrackID()));
            Hit->SetTime((*SpectrometerC)[i]->GetTime());
            Hit->SetEnergy((*SpectrometerC)[i]->GetEnergy());
            Hit->SetWireDistance((*SpectrometerC)[i]->GetWireDistance());
            G4ThreeVector Position = (*SpectrometerC)[i]->GetPosition();
            G4ThreeVector LocalPosition = (*SpectrometerC)[i]->GetLocalPosition();
            G4ThreeVector Direction = (*SpectrometerC)[i]->GetDirection();
            Hit->SetPosition(TVector3(Position[0],Position[1],Position[2]));
            Hit->SetLocalPosition(TVector3(LocalPosition[0],LocalPosition[1],LocalPosition[2]));
            Hit->SetDirection(TVector3(Direction[0],Direction[1],Direction[2]));
          }
        }
      }
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
