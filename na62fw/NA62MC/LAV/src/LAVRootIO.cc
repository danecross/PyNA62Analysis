// LAVRootIO.cc
// --------------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch)
// 2009-03-02 Emanuele Leonardi (Emanuele.Leonardi@roma1.infn.it)
//   - handle LAV-specific hit info
// 2010-03-15 - Domenico Di Filippo (difilippo@na.infn.it)
//   - Optical photons on photocatode
// 2011-01-24 - Domenico Di Filippo (difilippo@na.infn.it)
//   - Manage the new std::vector in LAVHit
//
// --------------------------------------------------------------------

#include "LAVRootIO.hh"
#include "RootIOManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "LAVGeometryParameters.hh"
#include "LAVMaterialParameters.hh"
#include "Event.hh"
#include "MCTruthManager.hh"
#include "TLAVEvent.hh"
#include "TLAVHit.hh"
#include "LAVSD.hh"
#include "TProcessID.h"

LAVRootIO::LAVRootIO() :
  NA62VRootIO(G4String("LAV")),
  fLAVBranch(nullptr)
{
  // Let's give some debug info
  //fVerbose = 2;

  fGeoPars = LAVGeometryParameters::GetInstance();
  fMatPars = LAVMaterialParameters::GetInstance();

  // Create run and event objects
  fEvent = new TLAVEvent();
  fEvent->SetIsMC(kTRUE);

  TTree::SetBranchStyle(fBranchStyle);

  G4cout << "LAVRootIO: Initialized" << G4endl;
}

LAVRootIO::~LAVRootIO() {
  delete fEvent;
}

void LAVRootIO::Close() {}

void LAVRootIO::NewRun() {
  // Create a branch to hold LAV Hits
  fEventTree = RootIOManager::GetInstance()->GetEventTree();
  fLAVBranch = fEventTree->Branch("LAV", fEvent->IsA()->GetName(), &fEvent);
  fLAVBranch->SetAutoDelete(kFALSE);
}

void LAVRootIO::EndRun() {
  if (fVerbose) G4cout << "LAVRootIO: Executing End-of-Run procedure" << G4endl;
}

void LAVRootIO::SaveEvent(const G4Event* eventG4) {
  if (fVerbose>=2) G4cout << "LAVRootIO: Preparing event structure" << G4endl;

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
    if(fVerbose>=2) G4cout << "LAVRootIO: Found hits collection " << HCname << G4endl;
    if (HCname == "LAVCollection"){
      LAVHitsCollection* LAVC = static_cast<LAVHitsCollection*>((LHC->GetHC(iHC)));
      if(LAVC) {
        int n_hit = LAVC->entries();
        if(n_hit>0){
          for(G4int i=0;i<n_hit;i++) {

            TLAVHit* Hit = static_cast<TLAVHit*>(fEvent->AddHit());
            Hit->SetMCTrackID(MCTruthManager::GetInstance()->FindKinePartIndex((*LAVC)[i]->GetTrackID()));
            Hit->SetDirectInteraction(MCTruthManager::GetInstance()->DirectInteraction((*LAVC)[i]->GetTrackID()));

            Hit->SetChannelID((*LAVC)[i]->GetChannelID());
            Hit->DecodeChannelID();

            Hit->SetPosition(TVector3((*LAVC)[i]->GetPosition()[0],
                  (*LAVC)[i]->GetPosition()[1],
                  (*LAVC)[i]->GetPosition()[2])
                );
            Hit->SetLocalPosition(TVector3((*LAVC)[i]->GetLocalPosition()[0],
                  (*LAVC)[i]->GetLocalPosition()[1],
                  (*LAVC)[i]->GetLocalPosition()[2])
                );
            Hit->SetLocalDirection(TVector3((*LAVC)[i]->GetLocalDirection()[0],
                  (*LAVC)[i]->GetLocalDirection()[1],
                  (*LAVC)[i]->GetLocalDirection()[2])
                );
            Hit->SetBeta((*LAVC)[i]->GetBeta());
            Hit->SetStepLength((*LAVC)[i]->GetStepLength());
            Hit->SetTime((*LAVC)[i]->GetTime());
            Hit->SetEnergy((*LAVC)[i]->GetEnergy());
            int PhotonsNumber = (*LAVC)[i]->GetPhotonsEnergy()->size();
            if (PhotonsNumber > (int)(*LAVC)[i]->GetPhotonsTime()->size())
              PhotonsNumber = (*LAVC)[i]->GetPhotonsTime()->size();
            Hit->SetPhotonsNumber(PhotonsNumber);
            for (int j=0; j<__TLAVHit_MAX_PHOTONS__; j++){
              if (j<PhotonsNumber){
                Hit->GetPhotonsEnergy()[j] = (*((*LAVC)[i]->GetPhotonsEnergy()))[j];
                Hit->GetPhotonsTime()[j] = (*((*LAVC)[i]->GetPhotonsTime()))[j] - (*LAVC)[i]->GetTime();
              } else {
                Hit->GetPhotonsEnergy()[j] = 0;
                Hit->GetPhotonsTime()[j] = 0;
              }
            }
          }
        }
      }
    }
  }

  TProcessID::SetObjectCount(savedObjNumber);

}
