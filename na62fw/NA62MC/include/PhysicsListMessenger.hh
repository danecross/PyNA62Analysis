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
//
// $Id: PhysicsListMessenger.hh,v 1.3.6.1 2010/03/18 10:33:19 gcosmo Exp $
// GEANT4 tag $Name: geant4-09-03-patch-02 $
//
//
/////////////////////////////////////////////////////////////////////////
//
// PhysicsListMessenger
//
// Created: 31.01.2006 V.Ivanchenko
//
// Modified:
// 04.06.2006 Adoptation of hadr01 (V.Ivanchenko)
//
//
// 03-09-2012 Sergey Podolsky (siarhei.padolski@cern.ch)
//
////////////////////////////////////////////////////////////////////////

#ifndef PhysicsListMessenger_h
#define PhysicsListMessenger_h 1

#include "globals.hh"
#include "G4UImessenger.hh"

class PhysicsList;
class G4UIcmdWithADouble;
class G4UIcmdWithAnInteger;
class G4UIcmdWithADoubleAndUnit;
class G4UIcmdWithAString;
class G4UIcmdWithoutParameter;

class PhysicsListMessenger: public G4UImessenger {

public:

  explicit PhysicsListMessenger(PhysicsList* p = 0);
  virtual ~PhysicsListMessenger();

  void SetNewValue(G4UIcommand*, G4String);
  void SetExoticParticleParameters();

private:

  PhysicsList* fPhysicsList;

  G4UIcmdWithADoubleAndUnit* fGammaCutCmd;
  G4UIcmdWithADoubleAndUnit* fElectCutCmd;
  G4UIcmdWithADoubleAndUnit* fPosCutCmd;
  G4UIcmdWithADoubleAndUnit* fCutCmd;
  G4UIcmdWithADoubleAndUnit* fAllCutCmd;
  G4UIcmdWithAString*        fPListCmd;
  G4UIcmdWithoutParameter*   fListCmd;  
  G4UIcmdWithoutParameter*   fFastCmd; 
  
  G4UIcmdWithADouble*        fExoticParticleMassCmd;
  G4UIcmdWithADouble*        fExoticParticleMassStepCmd;
  G4UIcmdWithAnInteger*      fExoticParticleNumberOfGeneratedParticlesCmd;
  G4UIcmdWithAnInteger*      fExoticParticleDecayModeCmd;
  G4UIcmdWithADouble*        fExoticParticleLifetimeCmd;
  G4UIcmdWithADouble*        fDecayPiplusDecayCmd;
  G4UIcmdWithAnInteger*      fMuonDecayCmd;

  G4double fExoticParticleInitialMass;
  G4double fExoticParticleMassStep;
  G4double fExoticParticleLifetime;
  G4int    fExoticParticleNumberOfGeneratedParticles;
  G4int    fExoticParticleDecayMode;

};

#endif
