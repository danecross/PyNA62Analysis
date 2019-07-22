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
// Modified by Cari Cesarotti 2017-02-04
// Modified by Sergey Podolsky 2013-10-25
// Modified by Giuseppe Ruggiero 2012-01-30
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-03-03
// 
// --------------------------------------------------------------

#include "PrimaryGeneratorMessenger.hh"
#include "PrimaryGeneratorAction.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithADouble.hh"

PrimaryGeneratorMessenger::PrimaryGeneratorMessenger(PrimaryGeneratorAction* Action) :
  fAction(Action), fExoticProdCmd(nullptr) {

  fBeamCmd = new G4UIcmdWithAString("/beam/SetBeam",this);
  fBeamCmd->SetGuidance("Select particle source type(Turtle)");
  fBeamCmd->SetGuidance("  turtle: na62 beam");
  fBeamCmd->SetGuidance("  gps: general purpose particle source");
  fBeamCmd->SetGuidance("  matrixgun: use for LAV block matrix generation");
  fBeamCmd->SetGuidance("  exotic: can generateAxion, HNL, A'");
  fBeamCmd->SetGuidance("  KL: neutral kaons at the decay volume entrance");
  fBeamCmd->SetGuidance("  hnl: heavy neutral lepton from charm production in the Be target");
  fBeamCmd->SetGuidance("  external: particles from the external file with pre-defined pdg_code,x,y,z,px,py,pz");
  fBeamCmd->SetGuidance("  input from G4BeamLine simulation");
  fBeamCmd->SetDefaultValue("turtle");
  fBeamCmd->SetCandidates("turtle gps matrixgun exotic replay KL hnl external g4blout");
  fBeamCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fTurtleParticleNameCmd = new G4UIcmdWithAString("/beam/ParticleName", this);
  fMaxKaonMomentum       = new G4UIcmdWithADouble("/beam/MaxKaonMomentum", this);
  fReplayFileCmd         = new G4UIcmdWithAString("/beam/ReplayFile", this);

  fExternalParticlesFileCmd = new G4UIcmdWithAString("/beam/ExternalParticlesFile", this);
  fG4BeamStartZPositionCmd  = new G4UIcmdWithAString("/beam/G4BeamStartZPosition", this);
  fG4BeamLineInputFileCmd   = new G4UIcmdWithAString("/beam/G4BeamLineInputFile", this);
}

PrimaryGeneratorMessenger::~PrimaryGeneratorMessenger() {
  delete fBeamCmd;
  delete fTurtleParticleNameCmd;
  delete fMaxKaonMomentum;
  delete fReplayFileCmd;
  delete fExternalParticlesFileCmd;
  delete fG4BeamLineInputFileCmd;
  delete fG4BeamStartZPositionCmd;
}

void PrimaryGeneratorMessenger::SetNewValue(G4UIcommand* cmd, G4String val) {
  G4cout << cmd->GetCommandPath() << " " << val << G4endl;
  if (cmd == fBeamCmd) { // beam type
    if      (val=="turtle")    fAction->SetBeamType(0);
    else if (val=="gps")       fAction->SetBeamType(1);
    else if (val=="matrixgun") fAction->SetBeamType(2);
    else if (val=="exotic")    fAction->SetBeamType(4);
    else if (val=="replay")    fAction->SetBeamType(5);
    else if (val=="KL")        fAction->SetBeamType(6);
    else if (val=="hnl")       fAction->SetBeamType(50);
    else if (val=="external")  fAction->SetBeamType(51);
    else if (val=="g4blout")   fAction->SetBeamType(52);
  }
  else if (cmd == fTurtleParticleNameCmd) { // Particle type in the turtle beam
    fAction->SetParticleName(val);
  }
  else if (cmd == fMaxKaonMomentum) {
    fAction->SetMaxKaonMomentum(fMaxKaonMomentum->GetNewDoubleValue(val));
  }
  else if (cmd == fReplayFileCmd) {
    fAction->SetInputFileName(val);
  }
  else if (cmd == fExternalParticlesFileCmd) { // external file with the full path
    fAction->SetInputExternalParticlesFileName(val);
  }
  else if (cmd == fG4BeamStartZPositionCmd) { // Beam at Z
    fAction->SetG4BeamStartZPosition(val);
  }
  else if (cmd == fG4BeamLineInputFileCmd) { // use G4 beamline output as input
    fAction->SetG4BeamLineInputFileName(val);
  }
}
