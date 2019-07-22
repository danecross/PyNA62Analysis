// --------------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//
// --------------------------------------------------------------------

#include "CedarRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "CedarGeometryParameters.hh"
#include "CedarMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TCedarEvent.hh"
#include "TCedarHit.hh"
#include "CedarSD.hh"
#include "TProcessID.h"

CedarRootIO::CedarRootIO() :
  NA62VRootIO(G4String("Cedar")),
  fCedarBranch(nullptr)
{

  fGeoPars = CedarGeometryParameters::GetInstance();
  fMatPars = CedarMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TCedarEvent();
  fEvent->SetIsMC(kTRUE);
  TTree::SetBranchStyle(fBranchStyle);
  G4cout << "CedarRootIO: Initialized" << G4endl;
}

CedarRootIO::~CedarRootIO() {
  delete fEvent;
}

void CedarRootIO::Close() {}

void CedarRootIO::NewRun() {
  // Create a branch to hold Cedar Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fCedarBranch = fEventTree->Branch("Cedar", fEvent->IsA()->GetName(), &fEvent);
  fCedarBranch->SetAutoDelete(kFALSE);
}

void CedarRootIO::EndRun() {
  if (fVerbose) G4cout << "CedarRootIO: Executing End-of-Run procedure" << G4endl;
}

void CedarRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "CedarRootIO: Preparing Cedar event structure" << G4endl;

  // Save current Object count
  Int_t savedObjNumber = TProcessID::GetObjectCount();
  G4int nEvent = eventG4->GetEventID();
  fEvent->Clear("C");
  fEvent->SetID(nEvent);
  fEvent->SetRunID(G4RunManager::GetRunManager()->GetCurrentRun()->GetRunID());

  // Get list of hit collections in this event
  G4HCofThisEvent* LHC = eventG4->GetHCofThisEvent();
  G4int nHC            = LHC->GetNumberOfCollections();

  for (G4int iHC=0; iHC<nHC; iHC++) {

    // Handle each collection type with the right method
    G4String HCname = LHC->GetHC(iHC)->GetName();
    if (HCname != "CedarCollection") continue;
    CedarHitsCollection* CedarC = static_cast<CedarHitsCollection*>((LHC->GetHC(iHC)));
    if (!CedarC) continue;

    for (G4int ihit=0; ihit<CedarC->entries(); ihit++) {
      TCedarHit *Hit = static_cast<TCedarHit*>(fEvent->AddHit());
      Hit->SetMCTrackID
        (MCTruthManager::GetInstance()->FindKinePartIndex((*CedarC)[ihit]->GetTrackID()));
      Hit->SetDirectInteraction
        (MCTruthManager::GetInstance()->DirectInteraction((*CedarC)[ihit]->GetTrackID()));
      Hit->SetChannelID((*CedarC)[ihit]->GetPositionID());
      Hit->DecodeChannelID();
      Hit->SetPosition(TVector3((*CedarC)[ihit]->GetPosition()[0],
				(*CedarC)[ihit]->GetPosition()[1],
				(*CedarC)[ihit]->GetPosition()[2]));
      Hit->SetTime((*CedarC)[ihit]->GetTime());
      Hit->SetEnergy((*CedarC)[ihit]->GetEnergy()); // photon energy
      Hit->SetPMType((*CedarC)[ihit]->GetPMType());
    }
  }
  TProcessID::SetObjectCount(savedObjNumber);
}
