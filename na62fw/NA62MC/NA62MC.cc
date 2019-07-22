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
// Updated by Simone Schuchmann 2019-04-01
// When compiling with Geant4 visualization enabled, the Qt viewer is
// executed. Use StandardVisRun.mac as input macro which calls vis.mac 
// in NA62MC/macros to start visualiztion. Depending on sub-det, start-
// up time can be quite long. To be improved! See also /vis commands
// http://geant4-userdoc.web.cern.ch/geant4-userdoc/UsersGuides/
// ForApplicationDeveloper/html/Visualization/commandcontrol.html
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// Modified by Sergey Podolsky 2011-01-21
// --------------------------------------------------------------

//G4 headers
#include "G4RunManager.hh"
#include "G4UImanager.hh"
#include "G4GeometryManager.hh"
#ifdef G4VIS_USE
#include "G4VisExecutive.hh"
#include "G4VisManager.hh"
#include "G4UIExecutive.hh"
#endif

#include "DetectorConstruction.hh"
#include "PhysicsList.hh"
#include "PrimaryGeneratorAction.hh"
#include "RunAction.hh"
#include "EventAction.hh"
#include "SteppingAction.hh"
#include "SteppingVerbose.hh"
#include "MCTruthTrackingAction.hh"
#include "RootIOManager.hh"
#include "DatacardManager.hh"
#include "NA62Global.hh"
#include "NA62ConditionsService.hh"
#include <signal.h>

void sighandler(int sig){
  G4cerr << G4endl << "********************************************************************************" << G4endl;
  G4cerr << "Killed with Signal " << sig << G4endl << "Closing ROOT files ..." << G4endl; 
  G4RunManager::GetRunManager()->AbortRun();
  RootIOManager::GetInstance()->EndRun(0);
  RootIOManager::GetInstance()->Close();
  G4cerr << "... Done" << G4endl;
  G4cerr << G4endl << "********************************************************************************" << G4endl;
  G4GeometryManager::GetInstance()->OpenGeometry();
  delete G4RunManager::GetRunManager();
  exit(0);
}

int main(int argc, char** argv) {
  G4cout << "[" << TimeString() <<"] NA62MC started" << G4endl;
  if (argc<2) {
    G4cout <<"Usage: NA62MC <macro name> [runNumber] [randomSeed]" << G4endl;
    exit(kWrongConfiguration);
  }
  const char * macroname = argv[1];
  G4cout << "Macro file used: " << macroname << G4endl;
  G4int RunNumber = 6610;  // default value
  if (argc >= 3) RunNumber = atoi(argv[2]);
  int SeedNum = -1;
  if (argc == 4) SeedNum = atoi(argv[3]);

  G4bool RunNumberOK =
    (RunNumber>=1560 && RunNumber<=4173) || // 2015
    (RunNumber>=4174 && RunNumber<=6941) || // 2016
    (RunNumber>=6942 && RunNumber<=8306) || // 2017
    (RunNumber>=8307 && RunNumber<=9463);   // 2018
  if (!RunNumberOK) {
    G4cerr << "[NA62MC] Error: invalid run number (" << RunNumber << ")" << G4endl;
    exit(kWrongConfiguration);
  }
  NA62ConditionsService::GetInstance()->SetCurrentRunID(RunNumber);
  NA62ConditionsService::GetInstance()->SetCurrentBurstID(0);

  // trap signals to close datafiles in case of abnormal termination
  signal(SIGUSR1,sighandler);
  signal(SIGXCPU,sighandler);
  signal(SIGINT,sighandler);
  signal(SIGTERM,sighandler);
  signal(127,sighandler);
  
  // choose the Random engine
  CLHEP::HepRandom::setTheEngine(new CLHEP::RanecuEngine());

  // my Verbose output class
  G4VSteppingVerbose::SetInstance(new SteppingVerbose);

  // Construct the default run manager
  G4RunManager* runManager = new G4RunManager;

  // Set inputs from datacard (macro file) 
  DatacardManager::GetInstance()->SetMessenger();
  DatacardManager::GetInstance()->SetRunNumber(RunNumber);

  // set mandatory initialization classes
  DetectorConstruction* detector = new DetectorConstruction;
  runManager->SetUserInitialization(detector);

  // Standard physics list
  runManager->SetUserInitialization(new PhysicsList);

  // set mandatory user action class
  runManager->SetUserAction(new RunAction);
  EventAction* EvAct = new EventAction(SeedNum);

  runManager->SetUserAction(new PrimaryGeneratorAction(detector, *EvAct));
  runManager->SetUserAction(EvAct);

  // set MCTruth user action classes (one and only one active)
  runManager->SetUserAction(new MCTruthTrackingAction); // Track by Track Information (filtered by MCTruthConfig)

  // enable black hole region treatment
  // (MCTruth step by step action is disabled by default)
  runManager->SetUserAction(new SteppingAction(EvAct)); // Step by Step Information

  // Initialize G4 kernel
  //runManager->Initialize();  //postponed at the macro-level with command /run/initialize, for PhysicsList parsing

  // configure MCTruth handling
  MCTruthConfig* config = new MCTruthConfig;
  config->SetEMin(100.0*MeV);
  MCTruthManager::GetInstance()->SetConfig(config);

#ifdef G4VIS_USE
  G4UIExecutive* ui = NULL;
  G4VisExecutive*visManager = NULL;
  visManager = new G4VisExecutive;
  visManager->Initialize();
  ui = new G4UIExecutive(argc,argv);//Qt by default
#endif

  // Define UI session for batch and interactive mode.
  G4UImanager* UI = G4UImanager::GetUIpointer();  
  G4String command = "/control/execute ";
  G4String fileName = macroname;
  UI->ApplyCommand(command+fileName);

#ifdef G4VIS_USE
  if(ui){
    ui->SessionStart();
    delete ui;
    delete visManager;
  }
#endif

  RootIOManager::GetInstance()->Close();
  delete runManager;
  G4cout << "[" << TimeString() <<"] NA62MC finished" << G4endl;
  return 0;
}
