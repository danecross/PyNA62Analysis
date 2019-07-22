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
// Modified by Sergey Podolsky 2011-01-21
// Modified by Sergey Podolsky 2013-10-25
// --------------------------------------------------------------

#ifndef MCTruthManager_H 
#define MCTruthManager_H 1

#include "G4Types.hh"
#include "G4LorentzVector.hh"

#include "MCTruthConfig.hh"
#include "KinePart.hh"
#include "TRandom3.h"

class Event;

class MCTruthManager {

public:

  static MCTruthManager* GetInstance();

  void NewEvent();
  void PrintEvent();

  KinePart* AddParticle();
  KinePart* GetKinePart(G4int);
  
  void StoreRandomState(TRandom3* RandomDecayState, long *RanecuState);
  void CrossReferenceParticles();
  G4int FindKinePartIndex(G4int);
  G4bool DirectInteraction(G4int);

  Event*         GetEvent()                      { return fEvent;            }
  void           SetEvent(Event* value)          { fEvent = value;           }
  MCTruthConfig* GetConfig()                     { return fConfig;           }
  void           SetConfig(MCTruthConfig* value) { fConfig = value;          }
  G4int*         GetKinePartIndexMap()           { return fKinePartIndexMap; }
  G4int*         GetSavedParentIDMap()           { return fSavedParentIDMap; }

protected:

  MCTruthManager();
  virtual ~MCTruthManager() {}

private:

  Event* fEvent;
  MCTruthConfig* fConfig;
  G4int fKinePartIndexMap[1000000]; 
  G4int fSavedParentIDMap[1000000]; 
};

#endif // MCTruthManager_H
