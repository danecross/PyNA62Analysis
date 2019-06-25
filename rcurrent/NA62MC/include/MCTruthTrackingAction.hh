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
// Based on GEANT 4 - MCTruthTrackingAction class
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)
//            Francesca Bucci (Francesca.Bucci@cern.ch) 2008-01-17
//
// --------------------------------------------------------------

#ifndef MCTruthTrackingAction_H 
#define MCTruthTrackingAction_H 1

#include "G4SystemOfUnits.hh"
#include "G4UserTrackingAction.hh"
#include "MCTruthManager.hh"
#include "MCTruthTrackInformation.hh"

class MCTruthTrackingAction : public G4UserTrackingAction {
public:

  MCTruthTrackingAction() {}
  virtual ~MCTruthTrackingAction() {}
  void PreUserTrackingAction(const G4Track*);
  void PostUserTrackingAction(const G4Track*);

private:

  G4bool          fRejectAll; ///< Do not save KineParts for all secondaries?
  G4LorentzVector fInitialMomentum;
  G4LorentzVector fFinalMomentum;
  G4bool          trackToBeStored(const G4Track*);
};

#endif // MCTruthTrackingAction_H
