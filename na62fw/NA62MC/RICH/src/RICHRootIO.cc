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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// 2012-03-07 Modified by Antonio Cassese (Antonio.Cassese@cern.ch) to save channels ID in agreement with new MC
// --------------------------------------------------------------

#include "RICHRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "RICHGeometryParameters.hh"
#include "RICHMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TRICHEvent.hh"
#include "TRICHHit.hh"
#include "RICHPMTSD.hh"
#include "TProcessID.h"

RICHRootIO::RICHRootIO() :
  NA62VRootIO(G4String("RICH")),
  fRICHBranch(nullptr),
  fNChannels(0),
  fNPMs(0),
  fPMsPositions(nullptr),
  fInputDisplacementWRTXaxis(0.0),
  fPMTsDiskCenter(nullptr),
  fPMTsDiskCenter_Jura_lab(nullptr),
  fPMTsDiskCenter_Saleve_lab(nullptr)
{

  GeoPars = RICHGeometryParameters::GetInstance();
  MatPars = RICHMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TRICHEvent();
  fEvent->SetIsMC(kTRUE);
  fGeoIDs = GeoPars->GetGeoIDs();
  TTree::SetBranchStyle(fBranchStyle);
  G4cout << "RICHRootIO: Initialized" << G4endl;
}

RICHRootIO::~RICHRootIO() {
  delete fEvent;
}

void RICHRootIO::Close() {}

void RICHRootIO::NewRun() {
  // Create a branch to hold TDC Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fRICHBranch = fEventTree->Branch("RICH", fEvent->IsA()->GetName(), &fEvent);
  fRICHBranch->SetAutoDelete(kFALSE);

  fNChannels = GeoPars->GetNChannels();
  fNPMs = GeoPars->GetNPMs();
  //G4cout<<"fNPMs: "<<fNPMs<<G4endl;
  fPMsPositions=GeoPars->GetPMsPositions();
  fPMsIDs=GeoPars->GetPMsIDs();
  fInputDisplacementWRTXaxis=GeoPars->GetInputDisplacementWRTXaxis();
  fPMTsDiskCenter=GeoPars->GetPMTsDiskCenter();
  fPMTsDiskCenter_Jura_lab = GeoPars->GetPMTsDiskCenter_Jura_lab();
  fPMTsDiskCenter_Saleve_lab = GeoPars->GetPMTsDiskCenter_Saleve_lab();
}

void RICHRootIO::EndRun() {
  if (fVerbose) G4cout << "RICHRootIO: Executing End-of-Run procedure" << G4endl;
}

void RICHRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "RICHRootIO: Preparing event structure" << G4endl;

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
    if(fVerbose>=2) G4cout << "RICHRootIO: Found hits collection " << HCname << G4endl;
    if (HCname == "PMTCollection"){
      RICHPMTHitsCollection* PMTC = static_cast<RICHPMTHitsCollection*>((LHC->GetHC(iHC)));
      if(PMTC) {
        int n_hit = PMTC->entries();
        //G4cout<<"n_hit: "<<n_hit<<G4endl;
        //G4double EventOffset=0;
        if(n_hit>0){

          G4int iPM;
          for(G4int i=0;i<n_hit;i++) {

            iPM=(*PMTC)[i]->GetPositionID();

            TRICHHit * Hit = static_cast<TRICHHit*>(fEvent->AddHit());

            if((*PMTC)[i]->GetPosition()[0]>0){
              Hit->SetChannelID(fGeoIDs[iPM]);
            }else{
              Hit->SetChannelID(fGeoIDs[iPM-976]+100000);
            }
            Hit->DecodeChannelID();

            // G4cout<<"iPM: "<<iPM<<"  Geo address: "<<PMGeoIDs[iPM]<<G4endl;
            // G4cout << " RICHChannelID: "<<Hit->GetChannelID()<<" Seq ID: "<<Hit->GetChannelSeqID(Hit->GetChannelID())<<G4endl;
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*PMTC)[i]->GetTrackID()));
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*PMTC)[i]->GetTrackID()));
            Hit->SetPosition(TVector3((*PMTC)[i]->GetPosition()[0],
                  (*PMTC)[i]->GetPosition()[1],
                  (*PMTC)[i]->GetPosition()[2])
                );
            Hit->SetTime((*PMTC)[i]->GetTime());
            Hit->SetEnergy((*PMTC)[i]->GetEnergy());
          }
        }
      }
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
