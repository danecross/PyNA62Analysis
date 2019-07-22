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

#include "NA62Global.hh"
#include "ExoticParticleGunMessenger.hh"
#include "ExoticParticleGun.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithADouble.hh"
#include "G4UIcmdWithABool.hh"

ExoticParticleGunMessenger::ExoticParticleGunMessenger(ExoticParticleGun* Gun) {
  fParticleGun = Gun;
  fNewProcCmd = new G4UIcmdWithAString("/exoticBeam/NewMode", this);
  fNewProcCmd->SetGuidance("Enter exotic generation process");
  fNewProcCmd->SetDefaultValue("axion direct");
  fNewProcCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fMesonDaughterCmd = new G4UIcmdWithABool("/exoticBeam/addMuon", this);
  fMesonDaughterCmd->SetGuidance("Add muon from parent meson decay or not");
  fMesonDaughterCmd->SetDefaultValue(0);
  fMesonDaughterCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fDecayZMinCmd = new G4UIcmdWithADouble("/exoticBeam/setZmin", this);
  fDecayZMinCmd->SetGuidance("Set the minimum z for decay");
  fDecayZMinCmd->SetDefaultValue(100000);
  fDecayZMinCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fDecayZMaxCmd = new G4UIcmdWithADouble("/exoticBeam/setZmax", this);
  fDecayZMaxCmd->SetGuidance("Set the maximum z for decay");
  fDecayZMaxCmd->SetDefaultValue(180000);
  fDecayZMaxCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fCouplingECmd = new G4UIcmdWithADouble("/exoticBeam/couplingE", this);
  fCouplingECmd->SetGuidance("Enter HNL coupling constant to electron");
  fCouplingECmd->SetDefaultValue(0);
  fCouplingECmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fCouplingMCmd = new G4UIcmdWithADouble("/exoticBeam/couplingM", this);
  fCouplingMCmd->SetGuidance("Enter HNL coupling constant to muon");
  fCouplingMCmd->SetDefaultValue(0);
  fCouplingMCmd->AvailableForStates(G4State_PreInit, G4State_Idle);  

  fCouplingUCmd = new G4UIcmdWithADouble("/exoticBeam/couplingU", this); 
  fCouplingUCmd->SetGuidance("Enter A' coupling constant to photon");
  fCouplingUCmd->SetDefaultValue(0);
  fCouplingUCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fTargetOrTaxCmd = new G4UIcmdWithAString("/exoticBeam/origin", this); 
  fTargetOrTaxCmd->SetGuidance("Enter 'target' or 'TAX' for the origin of the first particle"); 
  fTargetOrTaxCmd->SetDefaultValue("target"); 
  fTargetOrTaxCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fExoticDCmd = new G4UIcmdWithAString("/exoticBeam/addDaughter", this); 
  fExoticDCmd->SetGuidance("Enter all daughters of the exotic decay ");
  fExoticDCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fPropExoticCmd = new G4UIcmdWithAString("/exoticBeam/propExotic", this);
  fPropExoticCmd->SetGuidance("Turn on to propagate exotic particle, off to insert decay daughters instead"); 
  fPropExoticCmd->AvailableForStates(G4State_PreInit, G4State_Idle);
}

ExoticParticleGunMessenger::~ExoticParticleGunMessenger(){
  delete fNewProcCmd;
  delete fMesonDaughterCmd;
  delete fDecayZMinCmd;
  delete fDecayZMaxCmd;
  delete fCouplingECmd;
  delete fCouplingMCmd;
  delete fCouplingUCmd;
  delete fTargetOrTaxCmd;
  delete fExoticDCmd;
  delete fPropExoticCmd;
}

void ExoticParticleGunMessenger::SetNewValue(G4UIcommand* command, G4String val) {
  G4cout << command->GetCommandPath() << " " << val << G4endl;
  if (command == fNewProcCmd) { //Setting the exotic generation modes
    fParticleGun->AddExoticProductionMode(val);
  }
  else if (command == fDecayZMinCmd) {//Setting min z decay length in mm
    fParticleGun->SetDecayZMin(fDecayZMinCmd->GetNewDoubleValue(val));
  }
  else if (command == fDecayZMaxCmd){
    fParticleGun->SetDecayZMax(fDecayZMaxCmd->GetNewDoubleValue(val));
  }
  if (command == fCouplingECmd) { //Setting the exotic generation modes
    fParticleGun->SetCouplingE(fCouplingECmd->GetNewDoubleValue(val));
  }
  if (command == fCouplingMCmd) {
    fParticleGun->SetCouplingM(fCouplingMCmd->GetNewDoubleValue(val));
  }
  if (command == fCouplingUCmd) {
    fParticleGun->SetCouplingU(fCouplingUCmd->GetNewDoubleValue(val));
    G4cout << "The coupling is set " << G4endl;
  }
  if (command == fTargetOrTaxCmd) {
    if (val=="target")
      fParticleGun->TaxOrTarget(val); 
    else if (val=="TAX")
      fParticleGun->TaxOrTarget(val);
    else { 
      G4cout << "[ExoticParticleGunMessenger] Error: invalid choice for origin of parent particle" << G4endl;
      exit(kWrongConfiguration);
    }
  }
  if (command == fExoticDCmd) {
    fParticleGun->AddDaughter(val);
  }
  if (command == fPropExoticCmd) {
    fParticleGun->PropExotic(val);
  }
  if (command == fMesonDaughterCmd) {
    fParticleGun->SetMesonDaughter(fMesonDaughterCmd->GetNewBoolValue(val));
  }
}
