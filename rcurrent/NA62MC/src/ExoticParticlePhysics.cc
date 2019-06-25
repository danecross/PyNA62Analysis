// --------------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (cjp@hep.ph.bham.ac.uk) and
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), October 2015
//
// Updated by Viacheslav Duk (Viacheslav.Duk@cern.ch), August 2017
//
// --------------------------------------------------------------------

#include "ExoticParticle.hh"
#include "ExoticParticlePhysics.hh"
#include "G4ParticleDefinition.hh"
#include "G4ProcessManager.hh"
#include "G4ParticleTable.hh"

/// \class ExoticParticlePhysics
/// \Brief
/// Definition of physics processs for the exotic particle
/// \EndBrief

ExoticParticlePhysics::ExoticParticlePhysics(const G4String& name) :
  G4VPhysicsConstructor(name) {}

ExoticParticlePhysics::~ExoticParticlePhysics() {}

void ExoticParticlePhysics::ConstructParticle() {
  for(G4int iParticle=0; iParticle<200; iParticle++)
    ExoticParticle::Definition(iParticle);
}

void ExoticParticlePhysics::ConstructProcess() {
  for (G4int iParticle=0; iParticle<200; iParticle++) {
    ExoticParticle* ExoPart = ExoticParticle::Definition(iParticle);

    // Beam particle manager
    G4ParticleDefinition* def = G4ParticleTable::GetParticleTable()->FindParticle("kaon0L");
    G4ProcessManager* pmanager = new G4ProcessManager(def);

    // Construct process
    G4ProcessVector *processList = def->GetProcessManager()->GetProcessList();
    G4int nProcess = processList->entries();
    for (G4int i=0; i<nProcess; i++) {
      G4VProcess* process = (*processList)(i);

      // Only use decay and transportation processes, stolen from kaon0L
      if (process->GetProcessName()=="Decay" || process->GetProcessName()=="Transportation") {
	G4int atRestIndx    = def->GetProcessManager()->GetAtRestIndex(process);
	G4int alongStepIndx = def->GetProcessManager()->GetAlongStepIndex(process);
	G4int postStepIndx  = def->GetProcessManager()->GetPostStepIndex(process);
	pmanager->AddProcess(process,atRestIndx,alongStepIndx,postStepIndx);
      }
    }
    ExoPart->SetProcessManager(pmanager);
  }
}
