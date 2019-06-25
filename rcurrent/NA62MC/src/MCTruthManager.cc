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
// Based on GEANT 4 - MCTruthManager class
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)
//            Francesca Bucci (Francesca.Bucci@cern.ch) 2008-01-17
//
// Modified by Sergey Podolsky 2011-01-21
// Modified by Sergey Podolsky 2013-10-25
// --------------------------------------------------------------

#include "MCTruthManager.hh"
#include "Event.hh"
#include "G4ParticleTable.hh"
#include "RootIOManager.hh"
#include "TRandom3.h"

static MCTruthManager* fInstance = 0;

MCTruthManager::MCTruthManager() :
  fEvent(nullptr),
  fConfig(nullptr)
{}

MCTruthManager* MCTruthManager::GetInstance() {
  if (!fInstance) fInstance = new MCTruthManager();
  return fInstance;
}

void MCTruthManager::NewEvent() {
  // Get the Event object to be stored on disk and clear it
  fEvent=RootIOManager::GetInstance()->GetEvent();
  fEvent->Clear();
}

KinePart* MCTruthManager::AddParticle() {
  KinePart* part = fEvent->AddKinePart();
  return part;
}

KinePart* MCTruthManager::GetKinePart(G4int iPart) {
  TClonesArray &KinePartsArray = *(fEvent->GetKineParts());
  KinePart* part = static_cast<KinePart*>(KinePartsArray[iPart]);
  return part;
}

void MCTruthManager::StoreRandomState(TRandom3* RandomDecayState, long *RanecuState) {
  fEvent->StoreRandomState(RandomDecayState, RanecuState);
}

void MCTruthManager::CrossReferenceParticles() {
  TClonesArray &KinePartsArray = *(fEvent->GetKineParts());
  for (int iPart = 0; iPart < fEvent->GetNKineParts(); iPart++) {
    KinePart *Child = static_cast<KinePart*>(KinePartsArray[iPart]);
    if(Child->GetID()<0)
      continue;
    fKinePartIndexMap[Child->GetID()] = iPart;
    for (int jPart = 0; jPart < fEvent->GetNKineParts(); jPart++) {
      KinePart *PossibleParent = static_cast<KinePart*>(KinePartsArray[jPart]);
      if (Child->GetParentID()==PossibleParent->GetID()) {
	Child->SetParentIndex(jPart);
	break;
      }
    }
  }
}

G4int MCTruthManager::FindKinePartIndex(G4int MCTrackID) {
  if (MCTrackID >= 0 && MCTrackID < 1000000) {
    if (fSavedParentIDMap[MCTrackID] >= 0 && fSavedParentIDMap[MCTrackID] < 1000000) {
      //G4cout << "MCTrackID = " << MCTrackID << G4endl;
      //G4cout << "SavedParentIDMap = " << fSavedParentIDMap[MCTrackID] << G4endl;
      //G4cout << "KinePartIndexMap = " << fKinePartIndexMap[SavedParentIDMap[MCTrackID]] << G4endl;
      return fKinePartIndexMap[fSavedParentIDMap[MCTrackID]];
    }
    else {
      G4cerr << "WARNING - Saved Parent index out of range: " << fSavedParentIDMap[MCTrackID] << G4endl;
      G4cerr << "MCTrackID = " << MCTrackID << G4endl;
      return -1;
    }
  }
  else {
    G4cerr << "WARNING - Saved ParentID out of range: " << MCTrackID << G4endl;
    return -1;
  }
}

G4bool MCTruthManager::DirectInteraction(G4int MCTrackID) {
  if (MCTrackID >= 0 && MCTrackID < 1000000) {
    return (MCTrackID == fSavedParentIDMap[MCTrackID]);
  }
  else {
    G4cerr << "WARNING - Saved ParentID out of range: " << MCTrackID << G4endl;
    return false;
  }
}

void MCTruthManager::PrintEvent() {
  fEvent->Print();
}
