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

#include "RootIOManager.hh"
#include "CedarRootIO.hh"
#include "CHANTIRootIO.hh"
#include "CHODRootIO.hh"
#include "GigaTrackerRootIO.hh"
#include "HACRootIO.hh"
#include "IRCRootIO.hh"
#include "LAVRootIO.hh"
#include "LKrRootIO.hh"
#include "MUV0RootIO.hh"
#include "MUV1RootIO.hh"
#include "MUV2RootIO.hh"
#include "MUV3RootIO.hh"
#include "NewCHODRootIO.hh"
#include "RICHRootIO.hh"
#include "SACRootIO.hh"
#include "SpectrometerRootIO.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "DetectorConstruction.hh"
#include "Stream.hh"
#include "Event.hh"
#include "TString.h"
#include "TProcessID.h"
#include "DatacardManager.hh"
#include "PrimaryGeneratorAction.hh"
#include "PhysicsList.hh"
#include "ExoticParticle.hh"
#include "GitRevision.hh"
#include "NA62Global.hh"

RootIOManager* RootIOManager::fInstance = 0;

RootIOManager::RootIOManager() :
  fStreamTree(nullptr),
  fEventTree(nullptr),
  fStreamBranch(nullptr),
  fEventBranch(nullptr)
{

  // Create run and event objects
  fStream = new Stream();
  fEvent = new Event();

  // Default output file parameters
  fBufSize = 64000; //size of output buffer
  fBranchStyle = 1; //new branch style by default
  fCompLevel = 1; //fast compression by default

  // Default fVerbose level
  fVerbose = 0;

  // Default file name
  fFileName = DatacardManager::GetInstance()->GetOutputFileName(); 
  fFileNameHasChanged = true;

  // Make sure initial file pointer is empty (mandatory!)
  fFile = 0;

  TTree::SetBranchStyle(fBranchStyle);

  G4cout << "RootIOManager: Initialized" << G4endl;

  // Add subdetectors persistency managers
  fRootIOList.push_back(new CedarRootIO);
  fRootIOList.push_back(new CHANTIRootIO);
  fRootIOList.push_back(new CHODRootIO);
  fRootIOList.push_back(new GigaTrackerRootIO);
  fRootIOList.push_back(new HACRootIO);
  fRootIOList.push_back(new IRCRootIO);
  fRootIOList.push_back(new LAVRootIO);
  fRootIOList.push_back(new LKrRootIO);
  fRootIOList.push_back(new MUV0RootIO);
  fRootIOList.push_back(new MUV1RootIO);
  fRootIOList.push_back(new MUV2RootIO);
  fRootIOList.push_back(new MUV3RootIO);
  fRootIOList.push_back(new NewCHODRootIO);
  fRootIOList.push_back(new RICHRootIO);
  fRootIOList.push_back(new SACRootIO);
  fRootIOList.push_back(new SpectrometerRootIO);

  fGVirtMem = new TGraph();
  fGVirtMem->SetName("VirtualMemory");
  fGVirtMem->SetTitle("Virtual memory usage");
  fGResMem = new TGraph();
  fGResMem->SetName("ResidentMemory");
  fGResMem->SetTitle("Resident memory usage");
}

RootIOManager* RootIOManager::GetInstance() {
  if (!fInstance) fInstance = new RootIOManager();
  return fInstance;
}

void RootIOManager::Close() {
  // Save latest data, clean run tree, and close file
  if (fFile != 0) {
    //fFile->cd();
    fStreamTree->Write();
    if (fVerbose>=3) fStreamTree->Print();
    delete fStreamTree;
    fGVirtMem->GetYaxis()->SetTitle("Virtual Memory (MB)");
    fGVirtMem->GetXaxis()->SetTitle("Processed Events");
    fGVirtMem->Write();
    delete fGVirtMem;
    fGResMem->GetYaxis()->SetTitle("Resident Memory (MB)");
    fGResMem->GetXaxis()->SetTitle("Processed Events");
    fGResMem->Write();
    delete fGResMem;
    fFile->Purge();
    fFile->Close();
    if (fVerbose) G4cout << "RootIOManager: I/O file closed" << G4endl;
  }
}

void RootIOManager::SetFileName(G4String newName) {
  if (fVerbose) G4cout << "RootIOManager: Setting file name to " << newName << G4endl;
  fFileName = newName;
  fFileNameHasChanged = true;
}

void RootIOManager::NewRun(G4int nRun) {

  if (fVerbose) G4cout << "RootIOManager: Initializing I/O for run " << nRun << G4endl;

  if (fFileNameHasChanged) {

    // Close old file (if any)
    if (fFile != 0) {
      if (fVerbose) G4cout << "RootIOManager: Closing old file" << G4endl;
      Close();
    }

    // Create new file to hold data
    if (fVerbose) G4cout << "RootIOManager: Creating new file " << fFileName << G4endl;
    fFile = TFile::Open(fFileName.c_str(),"RECREATE","NA62MC");
    fFileNameHasChanged = false;

    // Define basic file properties
    if (fVerbose>=2) G4cout << "RootIOManager: Setting file compression level to " << fCompLevel << G4endl;
    fFile->SetCompressionLevel(fCompLevel);

    // Create tree to hold runs
    if (fVerbose>=2) G4cout << "RootIOManager: Creating new Streams tree" << G4endl;
    fStreamTree = new TTree("Streams", "List of streams");
    //fStreamTree->SetAutoSave(1000000000);  // autosave when ~1 Gbyte written
    fStreamTree->SetAutoSave(1000000);  // autosave when ~1 Mbyte written
    fStreamTree->SetDirectory(fFile->GetDirectory("/"));

    // Create tree to hold the run content info
    fStreamBranch = fStreamTree->Branch("Stream", fStream->IsA()->GetName(), &fStream);
    fStreamBranch->SetAutoDelete(kFALSE);
  }

  // Create tree to hold all events in this run
  fEventTree = new TTree("MC","MC events tree");
  //fEventTree->SetAutoSave(1000000000);  // autosave when ~1 Gbyte written
  fEventTree->SetAutoSave(1000000);  // autosave when ~1 Mbyte written
  TTree::SetMaxTreeSize(19000000000);
  fEventTree->SetDirectory(fFile->GetDirectory("/"));

  RootIOList::iterator iRootIO(fRootIOList.begin());
  RootIOList::iterator endRootIO(fRootIOList.end());
  while (iRootIO!=endRootIO) {
    if ((*iRootIO)->GetEnabled()) (*iRootIO)->NewRun();
    ++iRootIO;
  }

  // Create branch to hold the whole event structure
  fEventBranch = fEventTree->Branch("Generated", fEvent->IsA()->GetName(), &fEvent);
  fEventBranch->SetAutoDelete(kFALSE);
}

void RootIOManager::EndRun(Int_t EventsGenerated) {
  if (fVerbose) G4cout << "RootIOManager: Executing End-of-Run procedure" << G4endl;
  // Dump tree for this run to file and erase it
  if (fEventTree) {
    fEventTree->Write();
    if (fVerbose>=3) fEventTree->Print();
    delete fEventTree;
  }
  RootIOList::iterator iRootIO(fRootIOList.begin());
  RootIOList::iterator endRootIO(fRootIOList.end());

  while (iRootIO!=endRootIO) {
    if((*iRootIO)->GetEnabled())
      (*iRootIO)->EndRun();
    ++iRootIO;
  }

  // Fill a MCInfo object with general information specified in the macro file
  DatacardManager *dcm = DatacardManager::GetInstance();
  const PrimaryGeneratorAction *pga =
    static_cast<const PrimaryGeneratorAction*>(G4RunManager::GetRunManager()->GetUserPrimaryGeneratorAction());
  const PhysicsList *pl = dynamic_cast<const PhysicsList*>(G4RunManager::GetRunManager()->GetUserPhysicsList());

  fStream->Clear();
  fStream->GetMCInfo().SetRevision(GetCurrentGitRevision());
  fStream->GetMCInfo().SetBeamType(pga->GetBeamType());
  fStream->GetMCInfo().SetFastSimulationMode(dcm->GetFastSimulationMode());
  fStream->GetMCInfo().SetBrPie2(pl->GetBrPie2());
  fStream->GetMCInfo().SetForcedDecay(dcm->GetDecayForce());
  fStream->GetMCInfo().SetForcedMuonDecay(dcm->GetMuonDecayForce());
  fStream->GetMCInfo().SetForcedPionDecay(dcm->GetPionDecayForce());
  fStream->GetMCInfo().SetDecayType(dcm->GetDecayType());
  fStream->GetMCInfo().SetRadCor(dcm->GetDecayRadcor());
  fStream->GetMCInfo().SetPiZeroDecay(dcm->GetDecayPizeroDecay());
  fStream->GetMCInfo().SetZDecayMin(dcm->GetDecayZmin());
  fStream->GetMCInfo().SetZDecayMax(dcm->GetDecayZmax());
  fStream->GetMCInfo().SetExoticParticleMass(ExoticParticle::Definition(0)->GetPDGMass());
  fStream->GetMCInfo().SetExoticParticleDecayMode(pl->GetExoticParticleDecayMode());
  fStream->GetMCInfo().SetExoticParticleLifetime(ExoticParticle::Definition(0)->GetPDGLifeTime());
  fStream->GetMCInfo().AddFileName("");
  fStream->GetMCInfo().AddRunNumber(dcm->GetRunNumber());
  fStream->GetMCInfo().AddRandomSeed(dcm->GetRandDecaySeed());
  fStream->GetMCInfo().AddNEvents(EventsGenerated);
  if (fVerbose>=2) fStream->Print();

  // Fill run tree
  fStreamTree->Fill();
}

void RootIOManager::SaveEvent(const G4Event* eventG4) {

  G4int nEvent = eventG4->GetEventID();
  fEvent->SetEventNumber(nEvent);

  if (fVerbose>=2) G4cout << "RootIOManager: Saving event structure to file" << G4endl;
  if (fVerbose>=8) fEvent->Print();
  if (fVerbose>=9) fEvent->PrintAll();

  RootIOList::iterator iRootIO(fRootIOList.begin());
  RootIOList::iterator endRootIO(fRootIOList.end());
  while (iRootIO!=endRootIO) {
    if ((*iRootIO)->GetEnabled()) (*iRootIO)->SaveEvent(eventG4);
    ++iRootIO;
  }
  fEventTree->Fill();

  // Virtual memory usage monitoring
  G4int DownscalingFactor = 1; // check virtual memory every N processed events
  if(!(nEvent%DownscalingFactor)){
	std::pair<G4double,G4double> memoryUsage = RootIOManager::GetInstance()->GetMemoryUsage();
    fGVirtMem->SetPoint(nEvent/DownscalingFactor, nEvent, memoryUsage.first);
    fGResMem->SetPoint(nEvent/DownscalingFactor, nEvent, memoryUsage.second);
  }
}

std::pair<G4double,G4double> RootIOManager::GetMemoryUsage(){
  std::ifstream file;
  file.open("/proc/self/stat");
  TString datum;
  G4double virtualMemory = 0.;
  G4double residentMemory = 0.;
  if (file.is_open()) {
    for (int i = 0; i < 23; i++) datum.ReadToDelim(file, ' ');
    //virtual memory is in bytes
    virtualMemory = datum.Atoll() / (1024. * 1024.);
    datum.ReadToDelim(file, ' ');
    //resident memory is in pages (4kb each)
    residentMemory = datum.Atoll()*4 / (1024.);
  }
  file.close();
  return std::make_pair(virtualMemory, residentMemory);
}

NA62VRootIO* RootIOManager::FindRootIO(G4String name){
  RootIOList::iterator iRootIO(fRootIOList.begin());
  RootIOList::iterator endRootIO(fRootIOList.end());
  while (iRootIO!=endRootIO) {
    if ((*iRootIO)->GetName() == name) return (*iRootIO);
    ++iRootIO;
  }
  return 0;
}
