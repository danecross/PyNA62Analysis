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
// --------------------------------------------------------------
// History:
//
// Created by Cari Cesarotti Feb 2017
// 
// --------------------------------------------------------------

#ifndef ExoticParticleGunMessenger_h
#define ExoticParticleGunMessenger_h 1

#include "G4UImessenger.hh"
#include "globals.hh"

class ExoticParticleGun; 
class G4UIcmdWithAString; 
class G4UIcmdWithADouble;
class G4UIcmdWithABool;

class ExoticParticleGunMessenger: public G4UImessenger {
public:
  
  explicit ExoticParticleGunMessenger(ExoticParticleGun*);
  virtual ~ExoticParticleGunMessenger();
  void SetNewValue(G4UIcommand*, G4String);

private:
  ExoticParticleGun* fParticleGun;
  G4UIcmdWithAString* fNewProcCmd;
  G4UIcmdWithABool*   fMesonDaughterCmd;
  G4UIcmdWithADouble* fDecayZMinCmd;
  G4UIcmdWithADouble* fDecayZMaxCmd;
  G4UIcmdWithADouble* fCouplingECmd;
  G4UIcmdWithADouble* fCouplingMCmd;
  G4UIcmdWithADouble* fCouplingUCmd;
  G4UIcmdWithAString* fTargetOrTaxCmd;
  G4UIcmdWithAString* fExoticDCmd;
  G4UIcmdWithAString* fPropExoticCmd;

};
#endif
