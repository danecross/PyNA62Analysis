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
// Based on GEANT 4 - PhysicsList class from Hadr01 example
// --------------------------------------------------------------
//
// Antonino Sergi (Antonino.Sergi@cern.ch)
//
//  03-09-2012 Sergey Podolsky (siarhei.padolski@cern.ch)      
//
//  30-05-2013 Roberto Piandani (roberto.piandani@cern.ch) 
//
//  21-08-2013 Michal Koval (michal.koval@cern.ch)
//
//  27-03-2014 Karim Massri (karim.massri@cern.ch)
//    Updated physics list for Geant 4 10 compatibility (from Hadr01 example)
//      - deprecated lists removed: 
//          - G4HadronDElasticPhysics
//          - G4HadronQElasticPhysics
//          - G4HadronHElasticPhysics
//          - G4QStoppingPhysics
//          - G4LHEPStoppingPhysics
//          - HadronPhysicsLHEP
//          - HadronPhysicsQGSC_BERT
//          - HadronPhysicsQGSC_CHIPS
//          - HadronPhysicsFTFP_BERT_TRV
//          - HadronPhysicsLHEP
//          - HadronPhysicsLHEP_EMV
//          - HadronPhysicsQGSP
//      - change of names:
//          - HadronPhysicsFTFP_BERT      -> G4HadronPhysicsFTFP_BERT
//          - HadronPhysicsFTF_BIC        -> G4HadronPhysicsFTF_BIC
//          - HadronPhysicsQGSP_BERT      -> G4HadronPhysicsQGSP_BERT
//          - HadronPhysicsQGSP_BERT_HP   -> G4HadronPhysicsQGSP_BERT_HP
//          - HadronPhysicsQGSP_BIC       -> G4HadronPhysicsQGSP_BIC
//          - HadronPhysicsQGSP_BIC_HP    -> G4HadronPhysicsQGSP_BIC_HP
//          - HadronPhysicsQGSP_FTFP_BERT -> G4HadronPhysicsQGSP_FTFP_BERT
//          - HadronPhysicsQGS_BIC        -> G4HadronPhysicsQGS_BIC
//      - new lists added:
//          - G4EmStandardPhysics_option4
//          - G4HadronElasticPhysicsXS
//          - G4HadronElasticPhysicsHP
//          - G4NeutronCrossSectionXS
//          - G4StoppingPhysics
//          - G4HadronPhysicsFTFP_BERT_HP
//
// Autumn 2015: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), exotic particle and pion decays.
//
// August 2017: Viacheslav Duk (Viacheslav.Duk@cern.ch), exotic particle update
//
//  09-10-2017 Karim Massri (karim.massri@cern.ch)
//    Custom muon decay class introduced
//
// --------------------------------------------------------------

#include "PhysicsList.hh"
#include "PhysicsListMessenger.hh"
#include "G4DecayPhysics.hh"
#include "G4EmStandardPhysics.hh"
#include "G4EmStandardPhysics_option4.hh"
#include "G4EmLivermorePhysics.hh"
#include "G4EmPenelopePhysics.hh"
#include "G4HadronElasticPhysics.hh"
#include "G4HadronElasticPhysicsXS.hh"
#include "G4HadronElasticPhysicsHP.hh"
#include "G4ChargeExchangePhysics.hh"
#include "G4NeutronTrackingCut.hh"
#include "G4NeutronCrossSectionXS.hh"
#include "G4StoppingPhysics.hh"
#include "G4IonPhysics.hh"
#include "G4EmExtraPhysics.hh"
#include "G4EmProcessOptions.hh"

#include "G4HadronPhysicsFTFP_BERT.hh"
#include "G4HadronPhysicsQGSP_BERT.hh"
#include "G4HadronPhysicsQGSP_FTFP_BERT.hh"
#include "G4LossTableManager.hh"
#include "G4EmSaturation.hh"
#include "G4ProcessManager.hh"
#include "G4ParticleTypes.hh"
#include "G4ParticleTable.hh"
#include "G4ProductionCuts.hh"
#include "G4HadronicProcessStore.hh"
#include "G4FastSimulationManagerProcess.hh"
#include "G4DecayTable.hh"
#include "G4ProcessTable.hh"
#include "G4Decay.hh"
#include "G4DecayWithSpin.hh"
#include "CustomMuonDecay.hh"
#include "G4MuonDecayChannelWithSpin.hh"
#include "G4MuonRadiativeDecayChannelWithSpin.hh"
#include "MuonRadiativeDecayChannelWithSpinWithEgammaCut.hh"

#include "G4PhaseSpaceDecayChannel.hh"
#include "G4ParticlePropertyTable.hh"
#include "G4PhysicalConstants.hh"
#include "ExoticParticle.hh"
#include "ExoticParticlePhysics.hh"

#include "NA62Global.hh"

PhysicsList* PhysicsList::fgInstance                  = nullptr;
G4int        PhysicsList::fNumberOfGeneratedParticles = 0;
G4double     PhysicsList::fExoticParticleMassStep     = 0.;

///////////////////////
// Static access method

PhysicsList* PhysicsList::GetInstance() {
  if (!fgInstance) fgInstance = new PhysicsList();
  return fgInstance;
}

PhysicsList::PhysicsList() :
  G4VModularPhysicsList(),
  fCerenkovProcess(nullptr), fScintillationProcess(nullptr),
  fAbsorptionProcess(nullptr), fRayleighScatteringProcess(nullptr),
  fBoundaryProcess(nullptr), fWLSProcess(nullptr),
  fEmPhysicsList(nullptr), fParticleList(nullptr),
  fExoticParticleDecayMode(0),
  fMessenger(nullptr), fBrPie2(0.0) {
  G4LossTableManager::Instance();
  defaultCutValue = 0.7*mm;
  fCutForGamma    = defaultCutValue;
  fCutForElectron = defaultCutValue;
  fCutForPositron = defaultCutValue;
  fCutForProton   = defaultCutValue;
  verboseLevel    = 1;

  fMessenger = new PhysicsListMessenger(this);

  // Particles
  fParticleList   = new G4DecayPhysics("decays");
  fExoticParticle = new ExoticParticlePhysics("decays");

  // EM physics
  fEmPhysicsList = new G4EmStandardPhysics();

  // HNL mode
  fMDS = 1968.47;
}

PhysicsList::~PhysicsList() {
  delete fMessenger;
  delete fParticleList;
  delete fExoticParticle;
  delete fEmPhysicsList;
  for (size_t i=0; i<fHadronPhys.size(); i++) {
    delete fHadronPhys[i];
  }
}

////////////////////////////////////////////////////////////////////////////////
// Set the properties of the exotic particle (can be heavy neutrino, axion, etc)

void PhysicsList::SetExoticParticleMass(G4double ExoticParticleMass, G4int iParticle) {
  G4ParticlePropertyTable* PropTable = G4ParticlePropertyTable::GetParticlePropertyTable();
  G4ParticlePropertyData* ExoticParticleData =
    PropTable->GetParticleProperty(ExoticParticle::Definition(iParticle));
  ExoticParticleData->SetPDGMass(ExoticParticleMass);
  PropTable->SetParticleProperty(*ExoticParticleData);
}

void PhysicsList::SetExoticParticleLifetime(G4double Lifetime, G4int iParticle) {
  if (Lifetime<0.0) {
    G4cout << "[PhysicsList] Error: negative exotic particle lifetime" << G4endl;
    exit(kWrongConfiguration);
  }
  ExoticParticle::Definition(iParticle)->SetPDGLifeTime(Lifetime*nanosecond);
}

void PhysicsList::SetExoticParticleDecayMode(G4int ExoticParticleDecayMode, G4int iParticle) {

  fExoticParticleDecayMode = ExoticParticleDecayMode;
  if (!ExoticParticleDecayMode) { // mode=0: stable particle
    ExoticParticle::Definition(iParticle)->SetPDGLifeTime(-999);
    ExoticParticle::Definition(iParticle)->SetPDGStable(kTRUE);
    return;
  }
  G4int Ndaughters = 0;
  G4String daughters[3];
  daughters[0] = daughters[1] = daughters[2] = "";
  switch (ExoticParticleDecayMode) {

    ///////////////////////////////////////////
    // Two-body decays of heavy neutral leptons

  case 1:
    Ndaughters = 2; daughters[0] = "pi+"; daughters[1] = "e-";
    break;
  case 2:
    Ndaughters = 2; daughters[0] = "pi-"; daughters[1] = "e+";
    break;
  case 3:
    Ndaughters = 2; daughters[0] = "pi+"; daughters[1] = "mu-";
    break;
  case 4:
    Ndaughters = 2; daughters[0] = "pi-"; daughters[1] = "mu+";
    break;
  case 5:
    Ndaughters = 2; daughters[0] = "pi0"; daughters[1] = "nu_e";
    break;
  case 11:
    Ndaughters = 2; daughters[0] = "rho+"; daughters[1] = "e-";
    break;
  case 12:
    Ndaughters = 2; daughters[0] = "rho-"; daughters[1] = "e+";
    break;
  case 13:
    Ndaughters = 2; daughters[0] = "rho+"; daughters[1] = "mu-";
    break;
  case 14:
    Ndaughters = 2; daughters[0] = "rho-"; daughters[1] = "mu+";
    break;

    /////////////////////////////////////////////
    // Three-body decays of heavy neutral leptons

  case 51:
    Ndaughters = 3; daughters[0] = "e+"; daughters[1] = "e-"; daughters[2] = "nu_e";
    break;
  case 52:
    Ndaughters = 3; daughters[0] = "mu+"; daughters[1] = "mu-"; daughters[2] = "nu_e";
    break;
  case 53:
    Ndaughters = 3; daughters[0] = "e+"; daughters[1] = "mu-"; daughters[2] = "nu_e";
    break;
  case 54:
    Ndaughters = 3; daughters[0] = "e-"; daughters[1] = "mu+"; daughters[2] = "nu_e";
    break;

    //////////////////////////////////////////
    // Decays of non-leptonic exotic particles

  case 101:
    Ndaughters = 2; daughters[0] = "e+"; daughters[1] = "e-";
    break;
  case 102:
    Ndaughters = 2; daughters[0] = "mu+"; daughters[1] = "mu-";
    break;
  case 103:
    Ndaughters = 2; daughters[0] = "pi+"; daughters[1] = "pi-";
    break;
  case 104:
    Ndaughters = 2; daughters[0] = "gamma"; daughters[1] = "gamma";
    break;
  }

  if (!Ndaughters) {
    G4cout << "[PhysicsList] Error: invalid exotic particle decay mode: " << ExoticParticleDecayMode << G4endl;
    exit(kWrongConfiguration);
  }

  // Check validity of daughters
  for (G4int i=0; i<Ndaughters; i++) {
    if (!G4ParticleTable::GetParticleTable()->FindParticle(daughters[i])) {
      G4cout << "[PhysicsList] Error: exotic particle daughter ("<<daughters[i]<<") not known" << G4endl;
      exit(kWrongConfiguration);
    }
  }

  // Check if the parent mass exceeds the sum of daughter masses
  G4double TotalDaughterMass = 0.0;
  for (G4int i=0; i<Ndaughters; i++) {
    TotalDaughterMass +=
      G4ParticleTable::GetParticleTable()->FindParticle(daughters[i])->GetPDGMass();
  }
  if (TotalDaughterMass >= ExoticParticle::Definition(iParticle)->GetPDGMass()) {
    G4cout << "[PhysicsList] Error: exotic particle mass ("<<
      ExoticParticle::Definition(iParticle)->GetPDGMass() <<
      " MeV) too small for the decay mode (";
    for (G4int i=0; i<Ndaughters; i++) {
      if (i) G4cout <<" ";
      G4cout << daughters[i];
    }
    G4cout << ")" << G4endl;
    exit(kWrongConfiguration);
  }

  if (ExoticParticle::Definition(iParticle)->GetPDGMass() >= fMDS) {
    G4cout << "[PhysicsList] Error: exotic particle mass ("<<
      ExoticParticle::Definition(iParticle)->GetPDGMass() <<
      " MeV) too large to be produced from any D meson species" << G4endl;
    exit(kWrongConfiguration);
  }

  // G4PhaseSpaceDecayChannel format: parent, BR, number of daughters, daughter names
  G4DecayTable* table = new G4DecayTable();
  table->Insert(new G4PhaseSpaceDecayChannel
		(Form("Exotic%d",iParticle),
		 1.0, Ndaughters, daughters[0], daughters[1], daughters[2]));
  ExoticParticle::Definition(iParticle)->SetDecayTable(table);
}

void PhysicsList::SetExoticParticleNumberOfGeneratedParticles(G4int N) {
  fNumberOfGeneratedParticles = N;
}

void PhysicsList::SetExoticParticleMassStep(G4double N) {
  fExoticParticleMassStep = N;
}

////////////////////////////////////////////////

void PhysicsList::ConstructParticle() {
  // Standard Geant4 particles
  fParticleList->ConstructParticle();

  // The exotic particle
  fExoticParticle->ConstructParticle();
}

////////////////////////////////////
// Set BR(pi->enu) and BR(pi-->munu)

void PhysicsList::SetBrPie2(G4double val) {

  G4double BrPie2  = val;
  G4double BrPimu2 = 1.0 - BrPie2;
  fBrPie2 = BrPie2; 

  if (BrPie2<0.0 || BrPie2>1.0) {
    G4cout << "[PhysicsList] Error: invalid Br(pi-->enu) = " << BrPie2 << G4endl;
    exit(kWrongConfiguration);
  }
  //G4cout << "[PhysicsList] Setting Br(pi-->enu) = " << BrPie2 << G4endl;

  G4DecayTable* PionPlusDecayTable = new G4DecayTable();
  PionPlusDecayTable->Insert(new G4PhaseSpaceDecayChannel("pi+", BrPimu2, 2, "mu+", "nu_mu"));
  PionPlusDecayTable->Insert(new G4PhaseSpaceDecayChannel("pi+", BrPie2,  2, "e+",  "nu_e" ));
  G4PionPlus::PionPlusDefinition()->SetDecayTable(PionPlusDecayTable);

  G4DecayTable* PionMinusDecayTable = new G4DecayTable();
  PionMinusDecayTable->Insert(new G4PhaseSpaceDecayChannel("pi-", BrPimu2, 2, "mu-", "anti_nu_mu"));
  PionMinusDecayTable->Insert(new G4PhaseSpaceDecayChannel("pi-", BrPie2,  2, "e-",  "anti_nu_e" ));
  G4PionMinus::PionMinusDefinition()->SetDecayTable(PionMinusDecayTable);
}

//////////////////////////////
// Polarized muon decay tables

void PhysicsList::SetMuonDecay(G4int MuonDecayMode) {

  if(MuonDecayMode<0 || MuonDecayMode>2) {
    G4cout << "[PhysicsList] Error: invalid MuonDecayMode = " << MuonDecayMode << G4endl;
    exit(kWrongConfiguration);
  }
  //G4cout << "[PhysicsList] Setting MuonDecayMode = " << MuonDecayMode << G4endl;

  G4DecayTable* MuonPlusDecayTable = new G4DecayTable();
  G4DecayTable* MuonMinusDecayTable = new G4DecayTable();
  if (MuonDecayMode==0) { //GEANT4 mu decay (polarization + 1st order radiative corrections)
    MuonPlusDecayTable->Insert(new G4MuonDecayChannelWithSpin("mu+",1.0));
    MuonMinusDecayTable->Insert(new G4MuonDecayChannelWithSpin("mu-",1.0));
  }
  else if (MuonDecayMode==1) { //GEANT4 mu decay (polarization + 1st order radiative corrections + radiative mu->enunug decay, Eg>10MeV)
    MuonPlusDecayTable->Insert(new G4MuonDecayChannelWithSpin("mu+",0.986));
    MuonMinusDecayTable->Insert(new G4MuonDecayChannelWithSpin("mu-",0.986));
    MuonPlusDecayTable->Insert(new MuonRadiativeDecayChannelWithSpinWithEgammaCut("mu+",0.014)); 
    MuonMinusDecayTable->Insert(new MuonRadiativeDecayChannelWithSpinWithEgammaCut("mu-",0.014));
  }
  else if (MuonDecayMode==2) { //GEANT4 mu decay (only radiative mu->enunug decay, Eg>10MeV)
    MuonPlusDecayTable->Insert(new MuonRadiativeDecayChannelWithSpinWithEgammaCut("mu+",1.0));
    MuonMinusDecayTable->Insert(new MuonRadiativeDecayChannelWithSpinWithEgammaCut("mu-",1.0));
  }
  else if (MuonDecayMode==3) { //Custom mu decay (polarization + IB consistently with the RK definition)
    //WARNING: this generator is not ready yet!
    //The current implementation is just a copy of G4MuonDecayChannelWithSpin!
    MuonPlusDecayTable->Insert(new CustomMuonDecay("mu+",1.0));
    MuonMinusDecayTable->Insert(new CustomMuonDecay("mu-",1.0));
  }
  G4MuonPlus::MuonPlusDefinition()->SetDecayTable(MuonPlusDecayTable);
  G4MuonMinus::MuonMinusDefinition()->SetDecayTable(MuonMinusDecayTable);
}

void PhysicsList::ConstructProcess() {
  AddTransportation();
  fEmPhysicsList->ConstructProcess();
  OpticalPhysics();
  fParticleList->ConstructProcess();
  fExoticParticle->ConstructProcess();
  for (size_t i=0; i<fHadronPhys.size(); i++) {
    fHadronPhys[i]->ConstructProcess();
  }

  G4HadronicProcessStore::Instance()->SetVerbose(0);
  // G4HadronicProcessStore::Instance()->Dump(1);

  ///////////////////////
  // Muon decay with spin

  G4DecayWithSpin* decayWithSpin = new G4DecayWithSpin();
  G4ProcessTable* processTable = G4ProcessTable::GetProcessTable();
  G4VProcess* Decay = processTable->FindProcess("Decay",G4MuonPlus::MuonPlus());
  G4ProcessManager* Manager = G4MuonPlus::MuonPlus()->GetProcessManager();
  if (Manager) {
    if (Decay) Manager->RemoveProcess(Decay);
    Manager->AddProcess(decayWithSpin);
    Manager->SetProcessOrdering(decayWithSpin, idxPostStep);
    Manager->SetProcessOrdering(decayWithSpin, idxAtRest);
  }
  Decay = processTable->FindProcess("Decay",G4MuonMinus::MuonMinus());
  Manager = G4MuonMinus::MuonMinus()->GetProcessManager();
  if (Manager) {
    if (Decay) Manager->RemoveProcess(Decay);
    Manager->AddProcess(decayWithSpin);
    Manager->SetProcessOrdering(decayWithSpin, idxPostStep);
    Manager->SetProcessOrdering(decayWithSpin, idxAtRest);
  }
}

////////////////////
// Optical processes

#include "G4Cerenkov.hh"
#include "G4Scintillation.hh"
#include "G4OpAbsorption.hh"
#include "G4OpRayleigh.hh"
#include "G4OpBoundaryProcess.hh"
#include "G4OpWLS.hh"

//#include "G4MultipleScattering.hh"
//#include "G4eIonisation.hh"
//#include "G4eBremsstrahlung.hh"
//#include "G4eplusAnnihilation.hh"

void PhysicsList::OpticalPhysics() {
  fCerenkovProcess           = new G4Cerenkov("Cerenkov");
  fScintillationProcess      = new G4Scintillation("Scintillation");
  fAbsorptionProcess         = new G4OpAbsorption();
  fRayleighScatteringProcess = new G4OpRayleigh();
  fBoundaryProcess           = new G4OpBoundaryProcess();
  fWLSProcess 	             = new G4OpWLS();

  //fCerenkovProcess->DumpPhysicsTable();
  //fScintillationProcess->DumpPhysicsTable();
  //fAbsorptionProcess->DumpPhysicsTable();
  //fRayleighScatteringProcess->DumpPhysicsTable();
  //SetVerbose(1);
  //fBoundaryProcess->SetVerboseLevel(1);

  fCerenkovProcess->SetMaxNumPhotonsPerStep(30);
  fCerenkovProcess->SetTrackSecondariesFirst(false);

  fScintillationProcess->SetScintillationYieldFactor(1.);
  fScintillationProcess->SetTrackSecondariesFirst(false);

  // Use Birks Correction in the Scintillation process

  G4EmSaturation* emSaturation = G4LossTableManager::Instance()->EmSaturation();
  fScintillationProcess->AddSaturation(emSaturation);

  // Removed because obsolete (Geant 9.6) RP
  // fBoundaryProcess->SetModel(themodel);

#ifndef G4SLC6
  auto theParticleIterator=GetParticleIterator();
#endif
  theParticleIterator->reset();
  while( (*theParticleIterator)() ){
    G4ParticleDefinition* particle = theParticleIterator->value();
    G4ProcessManager* pmanager = particle->GetProcessManager();
    G4String particleName = particle->GetParticleName();
    if (particleName == "opticalphoton") {
      G4cout << " AddDiscreteProcess to OpticalPhoton " << G4endl;
      pmanager->AddDiscreteProcess(fAbsorptionProcess);
      //pmanager->AddDiscreteProcess(fRayleighScatteringProcess);
      pmanager->AddDiscreteProcess(fBoundaryProcess);
      pmanager->AddDiscreteProcess(fWLSProcess);
    }
    //if (particleName == "e+") {
    //  pmanager->AddProcess(new G4MultipleScattering, -1, 1, 1);
    //  pmanager->AddProcess(new G4eIonisation,        -1, 2, 2);
    //  pmanager->AddProcess(new G4eBremsstrahlung,    -1, 3, 3);
    //  pmanager->AddProcess(new G4eplusAnnihilation,   0,-1, 4);
    //}
    if (fScintillationProcess->IsApplicable(*particle)) {
      pmanager->AddProcess(fScintillationProcess);
      pmanager->SetProcessOrderingToLast(fScintillationProcess, idxAtRest);
      pmanager->SetProcessOrderingToLast(fScintillationProcess, idxPostStep);
    }
    if (fCerenkovProcess->IsApplicable(*particle)) {
      pmanager->AddProcess(fCerenkovProcess);
      pmanager->SetProcessOrdering(fCerenkovProcess,idxPostStep);
    }
  }
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo.....

void PhysicsList::AddPhysicsList(const G4String& name) {
  //if (verboseLevel>0) G4cout << "[PhysicsList] Adding list " << name << G4endl;
  if (name == "FTFP_BERT") {
    SetBuilderList1();
    fHadronPhys.push_back(new G4HadronPhysicsFTFP_BERT());
  }
  else if (name == "QGSP_BERT") {
    SetBuilderList1();
    fHadronPhys.push_back(new G4HadronPhysicsQGSP_BERT());
  }
  else if (name == "QGSP_FTFP_BERT") {
    SetBuilderList1();
    fHadronPhys.push_back(new G4HadronPhysicsQGSP_FTFP_BERT());
  }
  else if (name == "FTFP_BERT_EMZ") {
    delete fEmPhysicsList;
    fEmPhysicsList = new G4EmStandardPhysics_option4();
    AddPhysicsList("FTFP_BERT");
  }
  else if (name == "QGSP_BERT_EMZ") {
    delete fEmPhysicsList;
    fEmPhysicsList = new G4EmStandardPhysics_option4();
    AddPhysicsList("QGSP_BERT");
  }
  else if (name == "QGSP_FTFP_BERT_EMZ") {
    delete fEmPhysicsList;
    fEmPhysicsList = new G4EmStandardPhysics_option4();
    AddPhysicsList("QGSP_FTFP_BERT");
  }
  else {
    G4cout << "[PhysicsList] Invalid list " << name << G4endl;
    exit(kWrongConfiguration);
  }
}

void PhysicsList::SetBuilderList1(G4bool flagHP) {
  fHadronPhys.push_back(new G4EmExtraPhysics(verboseLevel));
  if (flagHP) fHadronPhys.push_back(new G4HadronElasticPhysicsHP(verboseLevel));
  else        fHadronPhys.push_back(new G4HadronElasticPhysics(verboseLevel));
  fHadronPhys.push_back(new G4StoppingPhysics(verboseLevel));
  fHadronPhys.push_back(new G4IonPhysics(verboseLevel));
  fHadronPhys.push_back(new G4NeutronTrackingCut(verboseLevel));
}

void PhysicsList::SetBuilderList2() {
  fHadronPhys.push_back(new G4EmExtraPhysics(verboseLevel));
  fHadronPhys.push_back(new G4HadronElasticPhysicsXS(verboseLevel));
  fHadronPhys.push_back(new G4StoppingPhysics(verboseLevel));
  fHadronPhys.push_back(new G4IonPhysics(verboseLevel));
  fHadronPhys.push_back(new G4NeutronTrackingCut(verboseLevel));
}

void PhysicsList::SetCuts() {
  if (verboseLevel>0) {
    G4cout << "[PhysicsList] CutLength " << G4BestUnit(fCutForGamma   ,"Length") << " for gamma"  << G4endl;
    G4cout << "[PhysicsList] CutLength " << G4BestUnit(fCutForElectron,"Length") << " for e-"     << G4endl;
    G4cout << "[PhysicsList] CutLength " << G4BestUnit(fCutForPositron,"Length") << " for e+"     << G4endl;
    G4cout << "[PhysicsList] CutLength " << G4BestUnit(fCutForProton  ,"Length") << " for proton" << G4endl;
  }

  // set cut values for gamma at first and for e- second and next for e+,
  // because some processes for e+/e- need cut values for gamma
  SetCutValue(fCutForGamma,    "gamma");
  SetCutValue(fCutForElectron, "e-");
  SetCutValue(fCutForPositron, "e+");
  SetCutValue(fCutForProton,   "proton");

  //   //cuts per region: radiator
  //   G4Region* RadiatorRegion = G4RegionStore::GetInstance()->GetRegion("RICHRadiator");
  //   G4ProductionCuts* RadiatorProductionCuts = new G4ProductionCuts();
  //   RadiatorProductionCuts->SetProductionCut(1.*mm,G4ProductionCuts::GetIndex("gamma"));
  //   RadiatorProductionCuts->SetProductionCut(10.*m,G4ProductionCuts::GetIndex("e-"));
  //   RadiatorProductionCuts->SetProductionCut(10.*m,G4ProductionCuts::GetIndex("e+"));
  //   RadiatorRegion->SetProductionCuts(RadiatorProductionCuts);

  //   //cuts per region: radiator
  //   G4Region* MirrorRegion = G4RegionStore::GetInstance()->GetRegion("RICHMirror");
  //   G4ProductionCuts* MirrorProductionCuts = new G4ProductionCuts();
  //   MirrorProductionCuts->SetProductionCut(1.*mm,G4ProductionCuts::GetIndex("gamma"));
  //   MirrorProductionCuts->SetProductionCut(1.*mm,G4ProductionCuts::GetIndex("e-"));
  //   MirrorProductionCuts->SetProductionCut(1.*mm,G4ProductionCuts::GetIndex("e+"));
  //   MirrorRegion->SetProductionCuts(MirrorProductionCuts);

  //   G4ProductionCutsTable::GetProductionCutsTable()->SetEnergyRange(100.*MeV, 200.*GeV);
  // if (verboseLevel>0) DumpCutValuesTable();
}

void PhysicsList::SetCutForGamma(G4double cut) {
  fCutForGamma = cut;
  SetParticleCuts(fCutForGamma, G4Gamma::Gamma());
}

void PhysicsList::SetCutForElectron(G4double cut) {
  fCutForElectron = cut;
  SetParticleCuts(fCutForElectron, G4Electron::Electron());
}

void PhysicsList::SetCutForPositron(G4double cut) {
  fCutForPositron = cut;
  SetParticleCuts(fCutForPositron, G4Positron::Positron());
}

void PhysicsList::SetCutForProton(G4double cut) {
  fCutForProton = cut;
  SetParticleCuts(fCutForProton, G4Proton::Proton());
}

void PhysicsList::AddParameterisation() {

  G4FastSimulationManagerProcess* fastSimProcess_massGeom =
    new G4FastSimulationManagerProcess("G4FSMP_massGeom", "World");
  
#ifndef G4SLC6
  auto theParticleIterator=GetParticleIterator();
#endif
  theParticleIterator->reset();
  while( (*theParticleIterator)() ) {
    G4ParticleDefinition* particle = theParticleIterator->value();
    G4ProcessManager* pmanager = particle->GetProcessManager();
    if (particle->GetParticleName() == "e+"  || 
        particle->GetParticleName() == "e-") {
      pmanager->AddDiscreteProcess(fastSimProcess_massGeom);
      pmanager->SetProcessOrdering(fastSimProcess_massGeom, idxAlongStep,  10);
      pmanager->SetProcessOrdering(fastSimProcess_massGeom, idxPostStep, 10 );
    }
  }
}
