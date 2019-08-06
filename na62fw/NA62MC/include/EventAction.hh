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
// Modified by Sergey Podolsky 2011-01-21
// --------------------------------------------------------------
#ifndef EventAction_h
#define EventAction_h 1

#include "G4UserEventAction.hh"
#include "globals.hh"
#include "TRandom3.h"
#include "Event.hh"
#include "TTree.h"
#include "TFile.h"
#include "NA62Random.hh"
#include "NA62Timer.hh"

class G4Event;

class EventAction : public G4UserEventAction {

public:

  explicit EventAction(int SeedNum);
  ~EventAction();

public:

  void BeginOfEventAction(const G4Event*);
  void EndOfEventAction(const G4Event*);
  void FillRandomEnginesStates();

  TRandom3* GetRandomDecayState() { return fRandomDecayState; }
  G4long    GetEventID()          { return fEventID;          }

  void SetSkip(Bool_t val) { fSkip = val; }
  void InitTimer(NA62Timer* timer, unsigned int tID) { fTimer = timer; fTimerID = tID; }

private:

  G4long fEventID;         ///< Event counter
  long fCurrentEventCount; ///< Special event counter for RandomEngineState
  NA62Random* fRandEvent;
  TRandom3* fRandomDecayState;
  long fRanecuState[2];
  G4bool fRandomEngineStateFileExist;
  TFile *fRandomEngineStateFile;
  TTree *fRandomEngineStateTree;
  G4int fCommandLineSeed;
  Bool_t fSkip; ///< Should this event be skipped, i.e. not saved?
  NA62Timer* fTimer;
  unsigned int fTimerID;
};

#endif
