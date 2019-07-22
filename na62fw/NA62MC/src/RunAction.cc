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
//
// --------------------------------------------------------------

#include "G4Timer.hh"
#include "RunAction.hh"
#include "G4Run.hh"
#include "RootIOManager.hh"
#include "Randomize.hh"
#include "G4RunManager.hh"
#include "DatacardManager.hh"
#include "PrimaryGeneratorAction.hh"
#include "NA62Global.hh"

RunAction::RunAction() {
  fTimer = new G4Timer();
}

RunAction::~RunAction() {
  delete fTimer;
}

void RunAction::BeginOfRunAction(const G4Run* aRun) {

  ///////////////////////////////////////////////
  // Protection against invalid datacard settings

  G4bool ParametersAreValid = kTRUE;
  DatacardManager::GetInstance()->SetRunBeamOn(aRun->GetNumberOfEventToBeProcessed());

  if (DatacardManager::GetInstance()->GetDecayForce()) {

    G4int DecayMode = DatacardManager::GetInstance()->GetDecayType();
    G4bool DecayModeIsValid =
      (DecayMode>=  0 && DecayMode<=  4) ||
      (DecayMode>= 10 && DecayMode<= 11) ||
      (DecayMode>= 20 && DecayMode<= 27) ||
      (DecayMode>= 30 && DecayMode<= 37) ||
      (DecayMode>= 40 && DecayMode<= 44) ||
      (DecayMode>= 60 && DecayMode<= 61) ||
      (DecayMode>= 70 && DecayMode<= 73) ||
      (DecayMode>= 80 && DecayMode<= 87) ||
      (DecayMode>=100 && DecayMode<=103) ||
      (DecayMode>=120 && DecayMode<=121) ||
      (DecayMode>=130 && DecayMode<=134) ||
      (DecayMode>=140 && DecayMode<=141) ||
      (DecayMode>=150 && DecayMode<=152) ||
      (DecayMode>=220 && DecayMode<=227) ||
      (DecayMode>=230 && DecayMode<=237) ||
      (DecayMode>=300 && DecayMode<=303) ||
      (DecayMode>=320 && DecayMode<=323) ||
      (DecayMode>=340 && DecayMode<=343);

    if (!DecayModeIsValid) {
      G4cerr << "[RunAction] ERROR: forced decay with invalid decay mode (" <<DecayMode<<")"<<G4endl;
      ParametersAreValid = kFALSE;
    }
    if (DatacardManager::GetInstance()->GetMuonDecayForce() &&
        DecayMode!=30 && DecayMode!=31 && DecayMode!=230 && DecayMode!=231) {
      G4cout << "[RunAction] ERROR: /decay/muforce 1 is valid only for /decay/type 30, 31, 230, 231" << G4endl;
      ParametersAreValid = kFALSE;
    }

    if (DatacardManager::GetInstance()->GetRadiativePhotonMinEnergy() > DatacardManager::GetInstance()->GetRadiativePhotonMinEnergy()){
      G4cout << "[RunAction] ERROR: /decay/RadiativePhotonMinEnergy > decay/RadiativePhotonMaxEnergy : wrong radiative photon energy range" << G4endl;
    }

    const PrimaryGeneratorAction *PGA =
      static_cast<const PrimaryGeneratorAction*>(G4RunManager::GetRunManager()->GetUserPrimaryGeneratorAction());
    G4int    BeamType         = PGA->GetBeamType();
    G4String BeamParticleName = PGA->GetParticleName();

    if (BeamType==0) { // Turtle beam type requested
      if (BeamParticleName=="kaon+" && DecayMode>=200) { // kaon beam
	G4cerr << "[RunAction] ERROR: beam particle is kaon+ but forced decay mode ("<<DecayMode<<") is not a K+ decay mode" << G4endl;
	ParametersAreValid = kFALSE;
      }
      if (BeamParticleName=="pi+" && (DecayMode<200 || DecayMode>=300)) { // pion beam
	G4cerr << "[RunAction] ERROR: beam particle is pi+ but forced decay mode ("<<DecayMode<<") is not a pi+ decay mode" << G4endl;
	ParametersAreValid = kFALSE;
      }
      if (BeamParticleName!="kaon+" && BeamParticleName!="pi+") {
	G4cerr << "[RunAction] ERROR: beam particle type is invalid for a forced decay" << G4endl;
	ParametersAreValid = kFALSE;
      }
    }
    if (BeamType==6 && DecayMode<300) { // KL beam is simulated
      G4cerr << "[RunAction] ERROR: KL beam is simulated but forced decay mode ("<<DecayMode<<") is not a KL decay mode" << G4endl;
      ParametersAreValid = kFALSE;
    }

    G4ThreeVector PizeroDecayMode = DatacardManager::GetInstance()->GetDecayPizeroDecayVector();
    G4bool PizeroDecayModeIsValid = kTRUE;
    G4double intPizeroDecayMode;
    for (G4int i=0; i<=2; i++) {
      if (std::modf(PizeroDecayMode[i], &intPizeroDecayMode) != 0.) PizeroDecayModeIsValid = kFALSE;
      PizeroDecayModeIsValid &=
	(PizeroDecayMode[i] >=  0 && PizeroDecayMode[i] <=  1) ||
	(PizeroDecayMode[i] >= 11 && PizeroDecayMode[i] <= 16) ||
	(PizeroDecayMode[i] >= 21 && PizeroDecayMode[i] <= 22) ||
	(PizeroDecayMode[i] >= 31 && PizeroDecayMode[i] <= 32) ||
	(PizeroDecayMode[i] >= 41 && PizeroDecayMode[i] <= 42) ||
	(PizeroDecayMode[i] >= 51 && PizeroDecayMode[i] <= 53) ||
	(PizeroDecayMode[i] == 90) ||
	(PizeroDecayMode[i] == 99);
    }

    if (!PizeroDecayModeIsValid) {
      G4cerr << "[RunAction] ERROR: Pi0 decay with invalid decay mode " << PizeroDecayMode << G4endl;
      ParametersAreValid = kFALSE;
    }
  }

  if (DatacardManager::GetInstance()->GetDecayZmin()>=
      DatacardManager::GetInstance()->GetDecayZmax()) {
    G4cout << "[RunAction] ERROR: Decay volume Z(min) > Z(max)" << G4endl;
    ParametersAreValid = kFALSE;
  }

  if (!ParametersAreValid) exit(kWrongConfiguration);

  // End of parameter validity check

  // Setting RunID
  const_cast<G4Run*>(aRun)->SetRunID(DatacardManager::GetInstance()->GetRunNumber());

  G4cout << "[" << TimeString() << "] Run " << aRun->GetRunID() << " start" << G4endl;
  fTimer->Start();

  // Enable ROOT output for this run
  // RootIOManager::GetInstance()->SetVerbose(1);

  RootIOManager::GetInstance()->NewRun(aRun->GetRunID());
  G4RunManager::GetRunManager()->RestoreRandomNumberStatus("RanecuInit.rndm");
  CLHEP::HepRandom::showEngineStatus();
}

void RunAction::EndOfRunAction(const G4Run* aRun) {
  fTimer->Stop();
  G4cout << "[" << TimeString() << "] Total events = " << aRun->GetNumberOfEvent() << " " << *fTimer << G4endl;

  // Finalize ROOT output for this run
  RootIOManager::GetInstance()->EndRun(aRun->GetNumberOfEvent());
  CLHEP::HepRandom::showEngineStatus();

  // Use this for long jobs to be able to restart from the last event
  // CLHEP::HepRandom::saveEngineStatus("RanecuInit.rndm");
}
