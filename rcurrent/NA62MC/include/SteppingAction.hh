//
// ********************************************************************
// * DISCLAIMER                                                       *
// *                                                                  *
// * The following disclaimer summarizes all the specific disclaimers *
// * of contributors to this software. The specific disclaimers,which *
// * govern, are listed with their locations in:                      *
// *   http://cern.ch/geant4/license                                  *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.                                                             *
// *                                                                  *
// * This  code  implementation is the  intellectual property  of the *
// * GEANT4 collaboration.                                            *
// * By copying,  distributing  or modifying the Program (or any work *
// * based  on  the Program)  you indicate  your  acceptance of  this *
// * statement, and all its terms.                                    *
// ********************************************************************

#ifndef SteppingAction_h
#define SteppingAction_h 1

#include "G4UserSteppingAction.hh"
#include "G4LorentzVector.hh"
#include "TString.h"
#include "G4SystemOfUnits.hh"

class EventAction;
class G4SteppingVerbose2;

class SteppingAction : public G4UserSteppingAction {
public:
  explicit SteppingAction(EventAction*);
  ~SteppingAction() {}
  void UserSteppingAction(const G4Step*);
  void PrintStep(const G4Step*, G4String ParticleName=""); ///< Detailed two-line printout
  void ShortPrintStep(const G4Step*, G4String ParticleName=""); ///< Brief one-line printout

private:
  EventAction* eventAction;
  G4SteppingVerbose2* myVerbose;
  G4LorentzVector fInitialMomentum;
  G4LorentzVector fFinalMomentum;

  G4int fNCheckPoints;
  TString fCheckPointNames[20], fVolumeName1[20], fVolumeName2[20];
  G4bool fPreStep[20];
};

#endif
