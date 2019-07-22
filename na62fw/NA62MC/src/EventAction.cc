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
// Modified by Sergey Podolsky 2013-02-14
// Modified by Sergey Podolsky 2012-03-08
// Modified by Giuseppe Ruggiero 2012-01-30
// Modified by Sergey Podolsky 2011-01-21
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// --------------------------------------------------------------

#include "EventAction.hh"
#include "RootIOManager.hh"
#include "G4Event.hh"
#include "G4EventManager.hh"
#include "G4TrajectoryContainer.hh"
#include "G4Trajectory.hh"
#include "G4VVisManager.hh"
#include "G4ios.hh"
#include "G4Timer.hh"
#include "MCTruthManager.hh"
#include "G4SDManager.hh"
#include "DatacardManager.hh"
#include "CMC.hh"
#include "PrimaryGeneratorAction.hh"

#include "G4RunManager.hh"
#include "RandomGenerator.hh"
#include <sys/types.h>
#include <sys/stat.h>

#include "NA62Global.hh"

EventAction::EventAction(int SeedNum) :
  timer(new G4Timer),
  fEventID(0), fCurrentEventCount(0),
  fRandomDecayState(nullptr), fRandomEngineStateFileExist(true),
  fCommandLineSeed(SeedNum), fSkip(false) {
  fRanecuState[0] = 0;
  fRanecuState[1] = 0;
  struct stat buffer;
  if (stat("./randomstate.root", &buffer) != 0) {
    fRandomEngineStateFileExist = false;
    CLHEP::HepRandom::setTheSeed(SeedNum);
  }
  else {
    fRandomEngineStateFile = TFile::Open("./randomstate.root");
    fRandomEngineStateTree = static_cast<TTree*>(fRandomEngineStateFile->Get("RandomStatesTree"));
    fRandEvent = new NA62Random();
    fRandomEngineStateTree->SetBranchAddress("rndm",&fRandEvent);
  }
}

EventAction::~EventAction() {
  delete timer;
  if (fRandomEngineStateFileExist) {
    delete fRandomDecayState;
    fRandomEngineStateFile->Close();
    delete fRandomEngineStateFile;
  }
}

void EventAction::BeginOfEventAction(const G4Event *evt) {
  timer->Start();
  fEventID = evt->GetEventID();
  fSkip = false;

  // Instantiating new event
  //MCTruthManager::GetInstance()->NewEvent();
}

void EventAction::EndOfEventAction(const G4Event* evt) {
  G4int event_id = evt->GetEventID();
  MCTruthManager::GetInstance()->StoreRandomState(fRandomDecayState, fRanecuState);

  // Save generated daughter momenta in their parents' rest frames.
  // This applies to decay products of 1) beam kaons and pions; 2) secondary pi0.
  for (G4int i=0; i<CMC::GetInstance()->GetNGeneratedParticles(); i++) {
    GenePart* part = MCTruthManager::GetInstance()->GetEvent()->AddGenePart();
    part->SetPDGcode(CMC::GetInstance()->GetGeneratedPDGCode(i));
    part->SetParticleName(CMC::GetInstance()->GetGeneratedName(i));
    part->SetInitialEnergy(CMC::GetInstance()->GetGeneratedEnergy(i));
    part->SetInitialMomentum(CMC::GetInstance()->GetGenerated3Momentum(i));
    part->SetParticleGroup(CMC::GetInstance()->GetFlag(i));
  }

  // Find the parent index for each particle
  MCTruthManager::GetInstance()->CrossReferenceParticles();

  // Print event
  // MCTruthManager::GetInstance()->PrintEvent();

  // Save event to root file
  if (!fSkip) RootIOManager::GetInstance()->SaveEvent(evt);

  // Stop event timer
  timer->Stop();

  // Get number of stored trajectories
  G4TrajectoryContainer* trajectoryContainer = evt->GetTrajectoryContainer();
  G4int n_trajectories = 0, n_optical = 0;
  if (trajectoryContainer) n_trajectories = trajectoryContainer->entries();

  if (G4VVisManager::GetConcreteInstance()) {
    for (G4int i=0; i<n_trajectories; i++) {

      // Get name of particle being drawn
      G4Trajectory* trj = static_cast<G4Trajectory*>(((*(evt->GetTrajectoryContainer()))[i]));
      G4String name = trj->GetParticleName();
      //G4cout << "EventAction: trajectory is a " << name << G4endl;

      // Use this to draw everything
      //trj->DrawTrajectory(0);
      if (name=="opticalphoton") n_optical++;
    }
  }

  // Periodic printing
  G4bool Print =
    (event_id<10) || (event_id<100 && !(event_id%10)) ||
    (event_id<1000 && !(event_id%100)) || !(event_id%1000);
  if (Print) {
    std::pair<G4double,G4double> memoryUsage = RootIOManager::GetInstance()->GetMemoryUsage();
    G4int pr = std::cout.precision();
    G4cout << std::fixed << std::setprecision(1);
    G4cout << "[" << TimeString() << "] Event " << evt->GetEventID() << " processed";
    G4cout << " [Memory usage -> virtual: " << std::setw(6) << memoryUsage.first << " MB,";
    G4cout << " resident: " << std::setw(6) << memoryUsage.second << " MB]" << std::setprecision(pr) << G4endl;
    std::cout.unsetf(std::ios::fixed);
  }

  // Use this for long jobs to be able to resubmit starting from the last event
  // CLHEP::HepRandom::saveEngineStatus("RanecuInit.rndm");
}

void EventAction::FillRandomEnginesStates() {

  if (fCurrentEventCount == 0) fRandomDecayState = new TRandom3();

  if (fRandomEngineStateFileExist && fRandomEngineStateTree->GetEntries() == fCurrentEventCount)
    G4cout << "###### ##### ##### Processing of the randomstate.root finished. Switching to pseudorandom event generation. ########## ######### #######" << G4endl;

  if ((!fRandomEngineStateFileExist) || (fRandomEngineStateTree->GetEntries() <= fCurrentEventCount)) {
    if (fCurrentEventCount == 0) {
      unsigned int iSeed = DatacardManager::GetInstance()->GetRandDecaySeed();
      if (fCommandLineSeed != -1) iSeed = fCommandLineSeed;
      G4cout << "[EventAction::FillRandomEnginesStates] Random seed = " << iSeed << G4endl;
      RandomGenerator::GetInstance()->Init(iSeed);
      // set the seeds for GPS particles to generate different events from run to run
      const PrimaryGeneratorAction *PGA =
              static_cast<const PrimaryGeneratorAction*>(G4RunManager::GetRunManager()->GetUserPrimaryGeneratorAction());
      G4int BeamType = PGA->GetBeamType();
      if (BeamType == 1) {
        G4cout << "===> GPS beam is used! " << G4endl;
        long gps_seeds[2];
        gps_seeds[0] = iSeed;
        gps_seeds[1] = iSeed + 62;
        CLHEP::HepRandom::setTheSeeds(gps_seeds);
      }
    }

    *fRandomDecayState = *RandomGenerator::GetInstance()->GetRandomDecay();
    const long *table = CLHEP::HepRandom::getTheSeeds();
    fRanecuState[0] = table[0];
    fRanecuState[1] = table[1];
  }
  else {
   if (fRandomEngineStateTree->GetEntries() > fCurrentEventCount) {
	fRandomEngineStateTree->GetEntry(fCurrentEventCount);
	fRanecuState[0] = fRandEvent->GetRanecuState()[0];
	fRanecuState[1] = fRandEvent->GetRanecuState()[1];
	CLHEP::HepRandom::setTheSeeds(fRanecuState);
	*fRandomDecayState = *fRandEvent->GetRandomDecayState();
	RandomGenerator::GetInstance()->Init(fRandomDecayState);
    }
   else
    G4RunManager::GetRunManager()->AbortRun();
  }
  fCurrentEventCount++;
}
