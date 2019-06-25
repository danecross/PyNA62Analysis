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
// $Id: PhysicsListMessenger.cc,v 1.5.2.1 2010/03/18 10:33:19 gcosmo Exp $
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
// 03-09-2012 Sergey Podolsky (siarhei.padolski@cern.ch)      
//
////////////////////////////////////////////////////////////////////////

#include "NA62Global.hh"
#include "PhysicsListMessenger.hh"
#include "PhysicsList.hh"
#include "G4UIcmdWithADouble.hh"
#include "G4UIcmdWithAnInteger.hh"
#include "G4UIcmdWithADoubleAndUnit.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithoutParameter.hh"
#include "G4UImanager.hh"
#include "G4RunManager.hh"

PhysicsListMessenger::PhysicsListMessenger(PhysicsList* pPhys) :
  G4UImessenger(), fPhysicsList(pPhys),
  fGammaCutCmd(0), fElectCutCmd(0), fPosCutCmd(0), fCutCmd(0), fAllCutCmd(0),
  fPListCmd(0), fListCmd(0), fFastCmd(0), fDecayPiplusDecayCmd(0), fMuonDecayCmd(0) {

  fGammaCutCmd = new G4UIcmdWithADoubleAndUnit("/Simulation/CutGamma",this);  
  fGammaCutCmd->SetGuidance("Set gamma cut.");
  fGammaCutCmd->SetParameterName("Gcut",false);
  fGammaCutCmd->SetUnitCategory("Length");
  fGammaCutCmd->SetRange("Gcut>=0.0");
  fGammaCutCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fElectCutCmd = new G4UIcmdWithADoubleAndUnit("/Simulation/CutEl",this);  
  fElectCutCmd->SetGuidance("Set electron cut.");
  fElectCutCmd->SetParameterName("Ecut",false);
  fElectCutCmd->SetUnitCategory("Length");
  fElectCutCmd->SetRange("Ecut>=0.0");
  fElectCutCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fPosCutCmd = new G4UIcmdWithADoubleAndUnit("/Simulation/CutPos",this);
  fPosCutCmd->SetGuidance("Set positron cut.");
  fPosCutCmd->SetParameterName("Pcut",false);
  fPosCutCmd->SetUnitCategory("Length");
  fPosCutCmd->SetRange("Pcut>=0.0");
  fPosCutCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fCutCmd = new G4UIcmdWithADoubleAndUnit("/Simulation/CutProt",this);
  fCutCmd->SetGuidance("Set proton cut.");
  fCutCmd->SetParameterName("ProtCut",false);
  fCutCmd->SetUnitCategory("Length");
  fCutCmd->SetRange("ProtCut>=0.0");
  fCutCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fAllCutCmd = new G4UIcmdWithADoubleAndUnit("/Simulation/CutsAll",this);
  fAllCutCmd->SetGuidance("Set cut for all.");
  fAllCutCmd->SetParameterName("cut",false);
  fAllCutCmd->SetUnitCategory("Length");
  fAllCutCmd->SetRange("cut>=0.0");
  fAllCutCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fPListCmd = new G4UIcmdWithAString("/Simulation/Physics",this);
  fPListCmd->SetGuidance("Add modula physics list.");
  fPListCmd->SetParameterName("PList",false);
  fPListCmd->AvailableForStates(G4State_PreInit);

  fListCmd = new G4UIcmdWithoutParameter("/Simulation/ListPhysics",this);
  fListCmd->SetGuidance("Available Physics Lists");
  fListCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fFastCmd = new G4UIcmdWithoutParameter("/Simulation/TurnOnFastLkr",this);
  fFastCmd->SetGuidance("Switch on fast LKr Simulation");
  fFastCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fExoticParticleMassCmd = new G4UIcmdWithADouble("/Simulation/ExoticParticle/Mass",this);
  fExoticParticleMassCmd->AvailableForStates(G4State_PreInit, G4State_Idle);  
  fExoticParticleMassStepCmd = new G4UIcmdWithADouble("/Simulation/ExoticParticle/MassStep",this);
  fExoticParticleMassStepCmd->AvailableForStates(G4State_PreInit, G4State_Idle);
  fExoticParticleNumberOfGeneratedParticlesCmd = new G4UIcmdWithAnInteger("/Simulation/ExoticParticle/NumberOfGeneratedParticles",this);
  fExoticParticleNumberOfGeneratedParticlesCmd->AvailableForStates(G4State_PreInit, G4State_Idle);
  fExoticParticleDecayModeCmd = new G4UIcmdWithAnInteger("/Simulation/ExoticParticle/DecayMode",this);
  fExoticParticleDecayModeCmd->AvailableForStates(G4State_PreInit, G4State_Idle);
  fExoticParticleLifetimeCmd = new G4UIcmdWithADouble("/Simulation/ExoticParticle/Lifetime",this);
  fExoticParticleLifetimeCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fDecayPiplusDecayCmd = new G4UIcmdWithADouble("/decay/BrPie2",this);
  fDecayPiplusDecayCmd->AvailableForStates(G4State_Idle);

  fMuonDecayCmd = new G4UIcmdWithAnInteger("/decay/muonDecay",this);
  fMuonDecayCmd->AvailableForStates(G4State_Idle);

  fExoticParticleInitialMass = -1.;
  fExoticParticleMassStep = -1.;
  fExoticParticleNumberOfGeneratedParticles = -1;
  fExoticParticleDecayMode = -1;
  fExoticParticleLifetime = -1;
}

PhysicsListMessenger::~PhysicsListMessenger() {
  delete fGammaCutCmd;
  delete fElectCutCmd;
  delete fPosCutCmd;
  delete fCutCmd;
  delete fAllCutCmd;
  delete fPListCmd;
  delete fListCmd;
  delete fFastCmd;
  delete fExoticParticleMassCmd;
  delete fExoticParticleDecayModeCmd;
  delete fExoticParticleLifetimeCmd;
  delete fDecayPiplusDecayCmd;
}

void PhysicsListMessenger::SetNewValue(G4UIcommand* command, G4String val) {

  G4cout << command->GetCommandPath() << " " << val << G4endl;

  G4UImanager* UI = G4UImanager::GetUIpointer();
  if (command == fGammaCutCmd) {
    if (fPhysicsList) {
      fPhysicsList->SetCutForGamma(fGammaCutCmd->GetNewDoubleValue(val));
    } else {
      UI->ApplyCommand("/run/setCutForAGivenParticle gamma " + val);
    }
  }
  else if (command == fElectCutCmd) {
    if(fPhysicsList) {
      fPhysicsList->SetCutForElectron(fElectCutCmd->GetNewDoubleValue(val));
    } else {
      UI->ApplyCommand("/run/setCutForAGivenParticle e- " + val);
    }
  }
  else if (command == fPosCutCmd) {
    if (fPhysicsList) {
      fPhysicsList->SetCutForPositron(fPosCutCmd->GetNewDoubleValue(val));
    } else {
      UI->ApplyCommand("/run/setCutForAGivenParticle e+ " + val);
    }
  } else if( command == fCutCmd ) {
    if (fPhysicsList) {
      fPhysicsList->SetCutForProton(fCutCmd->GetNewDoubleValue(val));
    } else {
      UI->ApplyCommand("/run/setCutForAGivenParticle proton " + val);
    }
  }
  else if (command == fAllCutCmd) {
    if (fPhysicsList) {
      G4double cut = fAllCutCmd->GetNewDoubleValue(val);
      fPhysicsList->SetCutForGamma(cut);
      fPhysicsList->SetCutForElectron(cut);
      fPhysicsList->SetCutForPositron(cut);
      fPhysicsList->SetCutForProton(cut);
    } else {
      UI->ApplyCommand("/run/setCut " + val);
    }
  }
  else if (command == fPListCmd) {
    if (fPhysicsList) {
      G4String name = val;
      if(name == "PHYSLIST") {
        char* path = getenv(name);
        if (path) name = G4String(path);
        else {
          G4cerr << "### PhysicsListMessenger WARNING: "
            << " environment variable PHYSLIST is not defined"
            << G4endl;
          return; 
        }
      }
      fPhysicsList->AddPhysicsList(name);
    } else {
      G4cerr << "### PhysicsListMessenger WARNING: "
        << " /Simulation/Physics UI command is not available "
        << "for reference Physics List" << G4endl;
    }
  }
  else if (command == fListCmd) {
    if (fPhysicsList) {
      fPhysicsList->List();
    } else { 
      G4cerr << "### PhysicsListMessenger WARNING: "
        << " /Simulation/ListPhysics UI command is not available "
        << "for reference Physics List" << G4endl;
    }
  }
  else if (command == fFastCmd) {
    if (fPhysicsList) {
      G4RunManager::GetRunManager()->GeometryHasBeenModified();
      fPhysicsList->AddParameterisation();
      G4RunManager::GetRunManager()->PhysicsHasBeenModified();
    } else { 
      G4cerr << "### PhysicsListMessenger WARNING: "
        << "/Simulation/TurnOnFastLkr UI command is not available "
        << "for reference Physics List" << G4endl;
    }
  }
  else if (command == fExoticParticleMassCmd) {
    fExoticParticleInitialMass = fExoticParticleMassCmd->GetNewDoubleValue(val);
    SetExoticParticleParameters();
  }
  else if (command == fExoticParticleMassStepCmd) {
    fExoticParticleMassStep = fExoticParticleMassStepCmd->GetNewDoubleValue(val);
    SetExoticParticleParameters();
  }
  else if (command == fExoticParticleNumberOfGeneratedParticlesCmd) {
    fExoticParticleNumberOfGeneratedParticles = fExoticParticleNumberOfGeneratedParticlesCmd->GetNewIntValue(val);
    if (fExoticParticleNumberOfGeneratedParticles<1 ||
	fExoticParticleNumberOfGeneratedParticles>200) {
      G4cerr << "[PhysicsListMessenger] Error: /Simulation/ExoticParticle/NumberOfGeneratedParticles outside 1-200 range" << G4endl;
      exit(kWrongConfiguration);
    }
    SetExoticParticleParameters();
  }
  else if (command == fExoticParticleDecayModeCmd) {
    fExoticParticleDecayMode = fExoticParticleDecayModeCmd->GetNewIntValue(val);
    SetExoticParticleParameters();
  }
  else if (command == fExoticParticleLifetimeCmd) {
    fExoticParticleLifetime = fExoticParticleLifetimeCmd->GetNewDoubleValue(val);
    SetExoticParticleParameters();
  }
  else if (command == fDecayPiplusDecayCmd) {
    if (fPhysicsList) {
      fPhysicsList->SetBrPie2(fDecayPiplusDecayCmd->GetNewDoubleValue(val));
    }
  }
  else if (command == fMuonDecayCmd) {
    if (fPhysicsList) {
      fPhysicsList->SetMuonDecay(fMuonDecayCmd->GetNewIntValue(val));
    }
  }
}

void PhysicsListMessenger::SetExoticParticleParameters() {

  // check if all parameters are read from the macro file
  if (fExoticParticleNumberOfGeneratedParticles>-1 &&
      fExoticParticleInitialMass>-1.0 &&
      fExoticParticleMassStep>-1. &&
      fExoticParticleDecayMode>-1 &&
      fExoticParticleLifetime>-1.) {

    fPhysicsList->SetExoticParticleNumberOfGeneratedParticles(fExoticParticleNumberOfGeneratedParticles);
    fPhysicsList->SetExoticParticleMassStep(fExoticParticleMassStep);
    for (G4int i=0; i<fExoticParticleNumberOfGeneratedParticles; i++) {
      fPhysicsList->SetExoticParticleMass(fExoticParticleInitialMass+i*fExoticParticleMassStep, i);
      fPhysicsList->SetExoticParticleDecayMode(fExoticParticleDecayMode, i);
      fPhysicsList->SetExoticParticleLifetime(fExoticParticleLifetime, i);
    }
  }
}
