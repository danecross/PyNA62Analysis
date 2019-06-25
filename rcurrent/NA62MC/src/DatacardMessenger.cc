#include "DatacardMessenger.hh"
#include "DatacardManager.hh"
#include "G4UIdirectory.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithADouble.hh"
#include "G4UIcmdWithAnInteger.hh"
#include "G4UIcmdWithABool.hh"
#include "G4UIcmdWith3Vector.hh"
#include "NA62Global.hh"

DatacardMessenger::DatacardMessenger(DatacardManager* datacardMng) :
  fDatacardManager(datacardMng) {

  // General/decay mode commands
  fOutputFileNameCmd = new G4UIcmdWithAString("/output/fileName",this);
  fOutputFileNameCmd->AvailableForStates(G4State_Idle);
  fDecayForceCmd = new G4UIcmdWithABool("/decay/force",this);
  fDecayForceCmd->AvailableForStates(G4State_Idle);
  fMuonDecayForceCmd = new G4UIcmdWithABool("/decay/muforce",this);
  fMuonDecayForceCmd->AvailableForStates(G4State_Idle);
  fPionDecayForceCmd = new G4UIcmdWithAnInteger("/decay/piforce",this);
  fPionDecayForceCmd->AvailableForStates(G4State_Idle);
  fDecayTypeCmd = new G4UIcmdWithAnInteger("/decay/type",this);
  fDecayTypeCmd->AvailableForStates(G4State_Idle);
  fDecayRadcorCmd = new G4UIcmdWithAnInteger("/decay/radcor",this);
  fDecayRadcorCmd->AvailableForStates(G4State_Idle);
  fDecayPizeroDecayCmd = new G4UIcmdWith3Vector("/decay/pizeroDecay",this);
  fDecayPizeroDecayCmd->AvailableForStates(G4State_Idle);
  fTwoPhotonsMaxAngle = new G4UIcmdWithADouble("/decay/twoPhotonsMaxAngle",this);
  fTwoPhotonsMaxAngle->AvailableForStates(G4State_Idle);
  fRadiativePhotonMinEnergy = new G4UIcmdWithADouble("/decay/RadiativePhotonMinEnergy",this);
  fRadiativePhotonMinEnergy->AvailableForStates(G4State_Idle);
  fRadiativePhotonMaxEnergy = new G4UIcmdWithADouble("/decay/RadiativePhotonMaxEnergy",this);
  fRadiativePhotonMaxEnergy->AvailableForStates(G4State_Idle);
  fLeptonPhotonMinAngle = new G4UIcmdWithADouble("/decay/LeptonPhotonMinAngle",this);
  fLeptonPhotonMinAngle->AvailableForStates(G4State_Idle);
  fMinTracksMomentum = new G4UIcmdWithADouble("/decay/MinTracksMomentum",this);
  fMinTracksMomentum->AvailableForStates(G4State_Idle);
  fDecayZMinCmd = new G4UIcmdWithADouble("/decay/zMin",this);
  fDecayZMinCmd->AvailableForStates(G4State_Idle);
  fDecayZMaxCmd = new G4UIcmdWithADouble("/decay/zMax",this);
  fDecayZMaxCmd->AvailableForStates(G4State_Idle);
  fDecayVerboseCmd = new G4UIcmdWithAnInteger("/decay/verbose",this);
  fDecayVerboseCmd->AvailableForStates(G4State_Idle);
  fRandomSeedCmd = new G4UIcmdWithADouble("/random/seed",this);  // unsigned int get overflow with G4UIcmdWithAnInteger
  fRandomSeedCmd->AvailableForStates(G4State_Idle);

  // Commands for control of KineParts saving/rejection
  fKineSaveAllStepsCmd = new G4UIcmdWithABool("/kine/SaveAllSteps",this);
  fKineSaveAllStepsCmd->AvailableForStates(G4State_Idle);
  fKineSaveAllTracksCmd = new G4UIcmdWithABool("/kine/SaveAllTracks",this);
  fKineSaveAllTracksCmd->AvailableForStates(G4State_Idle);

  fKineBeamParticleEndProcessToRejectCmd = new G4UIcmdWithAString("/kine/BeamParticleEndProcessToReject",this);
  fKineBeamParticleEndProcessToRejectCmd->AvailableForStates(G4State_Idle);
  fKineZMinCmd = new G4UIcmdWithADouble("/kine/zMinBeamForKineParts",this);
  fKineZMinCmd->AvailableForStates(G4State_Idle);
  fKineZMaxCmd = new G4UIcmdWithADouble("/kine/zMaxBeamForKineParts",this);
  fKineZMaxCmd->AvailableForStates(G4State_Idle);

  fKineParticleTypeToSaveCmd = new G4UIcmdWithAString("/kine/ParticleTypeToSave",this);
  fKineParticleTypeToSaveCmd->AvailableForStates(G4State_Idle);
  fKineCreatorProcessToSaveCmd = new G4UIcmdWithAString("/kine/CreatorProcessToSave",this);
  fKineCreatorProcessToSaveCmd->AvailableForStates(G4State_Idle);
  fKineEndProcessToSaveCmd = new G4UIcmdWithAString("/kine/EndProcessToSave",this);
  fKineEndProcessToSaveCmd->AvailableForStates(G4State_Idle);

  fKineParticleTypeToRejectCmd = new G4UIcmdWithAString("/kine/ParticleTypeToReject",this);
  fKineParticleTypeToRejectCmd->AvailableForStates(G4State_Idle);
  fKineCreatorProcessToRejectCmd = new G4UIcmdWithAString("/kine/CreatorProcessToReject",this);
  fKineCreatorProcessToRejectCmd->AvailableForStates(G4State_Idle);
  fKineEndProcessToRejectCmd = new G4UIcmdWithAString("/kine/EndProcessToReject",this);
  fKineEndProcessToRejectCmd->AvailableForStates(G4State_Idle); 

  fKineMinECmd = new G4UIcmdWithADouble("/kine/MinimumEnergy",this);
  fKineMinECmd->AvailableForStates(G4State_Idle);
  fKineMaxIntLevelCmd = new G4UIcmdWithAnInteger("/kine/MaximumInteractionLevel",this);
  fKineMaxIntLevelCmd->AvailableForStates(G4State_Idle); 
  fKineSaveVerboseCmd = new G4UIcmdWithABool("/kine/SaveTrackVerbose",this);
  fKineSaveVerboseCmd->AvailableForStates(G4State_Idle);

  fFastSimuSchemeCmd = new G4UIcmdWithAnInteger("/fastSimu/StepNo",this);
  fFastSimuSchemeCmd->AvailableForStates(G4State_Idle);
  fFastSimuSchemeInputFileCmd = new G4UIcmdWithAString("/fastSimu/InputFile",this);
  fFastSimuSchemeInputFileCmd->AvailableForStates(G4State_Idle);

  fFastSimulationModeCmd = new G4UIcmdWithAnInteger("/Detector/FastSimulationMode",this);
  fFastSimulationModeCmd->AvailableForStates(G4State_Idle);
}

DatacardMessenger::~DatacardMessenger() {
  delete fOutputFileNameCmd;
  delete fDecayForceCmd;
  delete fMuonDecayForceCmd;
  delete fPionDecayForceCmd;
  delete fDecayTypeCmd;
  delete fDecayRadcorCmd;
  delete fDecayPizeroDecayCmd;
  delete fTwoPhotonsMaxAngle;
  delete fRadiativePhotonMinEnergy;
  delete fRadiativePhotonMaxEnergy;
  delete fLeptonPhotonMinAngle;
  delete fMinTracksMomentum;
  delete fDecayZMinCmd;
  delete fDecayZMaxCmd;
  delete fDecayVerboseCmd;
  delete fRandomSeedCmd;

  delete fKineSaveAllStepsCmd;
  delete fKineSaveAllTracksCmd;
  delete fKineBeamParticleEndProcessToRejectCmd;
  delete fKineZMinCmd;
  delete fKineZMaxCmd;
  delete fKineParticleTypeToSaveCmd;
  delete fKineCreatorProcessToSaveCmd;
  delete fKineEndProcessToSaveCmd;
  delete fKineParticleTypeToRejectCmd;
  delete fKineCreatorProcessToRejectCmd;
  delete fKineEndProcessToRejectCmd;
  delete fKineMinECmd;
  delete fKineMaxIntLevelCmd;
  delete fKineSaveVerboseCmd;
  delete fFastSimuSchemeCmd;
  delete fFastSimuSchemeInputFileCmd;
  delete fFastSimulationModeCmd;
}

void DatacardMessenger::SetNewValue(G4UIcommand* cmd, G4String val) { 
  G4cout << cmd->GetCommandPath() << " " << val << G4endl;

  if (cmd==fOutputFileNameCmd)
    fDatacardManager->SetOutputFileName(val);
  else if (cmd==fDecayForceCmd)
    fDatacardManager->SetDecayForce(fDecayForceCmd->GetNewBoolValue(val));
  else if (cmd==fMuonDecayForceCmd)
    fDatacardManager->SetMuonDecayForce(fMuonDecayForceCmd->GetNewBoolValue(val));
  else if (cmd==fPionDecayForceCmd)
    fDatacardManager->SetPionDecayForce(fPionDecayForceCmd->GetNewIntValue(val));
  else if (cmd==fDecayTypeCmd)
    fDatacardManager->SetDecayType(fDecayTypeCmd->GetNewIntValue(val));
  else if (cmd==fDecayRadcorCmd)
    fDatacardManager->SetDecayRadcor(fDecayRadcorCmd->GetNewIntValue(val));
  else if (cmd==fDecayPizeroDecayCmd)
    fDatacardManager->SetDecayPizeroDecay(fDecayPizeroDecayCmd->GetNew3VectorValue(val));
  else if (cmd==fTwoPhotonsMaxAngle)
    fDatacardManager->SetTwoPhotonsMaxAngle(fTwoPhotonsMaxAngle->GetNewDoubleValue(val));
  else if (cmd==fRadiativePhotonMinEnergy)
    fDatacardManager->SetRadiativePhotonMinEnergy(fRadiativePhotonMinEnergy->GetNewDoubleValue(val));
  else if (cmd==fRadiativePhotonMaxEnergy)
    fDatacardManager->SetRadiativePhotonMaxEnergy(fRadiativePhotonMaxEnergy->GetNewDoubleValue(val));
  else if (cmd==fLeptonPhotonMinAngle)
    fDatacardManager->SetLeptonPhotonMinAngle(fLeptonPhotonMinAngle->GetNewDoubleValue(val));
  else if (cmd==fMinTracksMomentum)
    fDatacardManager->SetMinTracksMomentum(fMinTracksMomentum->GetNewDoubleValue(val)); 
  else if (cmd==fDecayZMinCmd)
    fDatacardManager->SetDecayZmin(fDecayZMinCmd->GetNewDoubleValue(val)); 
  else if (cmd==fDecayZMaxCmd)
    fDatacardManager->SetDecayZmax(fDecayZMaxCmd->GetNewDoubleValue(val));
  else if (cmd==fDecayVerboseCmd)
    fDatacardManager->SetDecayVerbose(fDecayVerboseCmd->GetNewIntValue(val));
  else if (cmd==fRandomSeedCmd)
    fDatacardManager->SetRandDecaySeed((unsigned int)fRandomSeedCmd->GetNewDoubleValue(val));
  else if (cmd==fKineSaveAllStepsCmd)
    fDatacardManager->SetSaveAllSteps(fKineSaveAllStepsCmd->GetNewBoolValue(val));
  else if (cmd==fKineSaveAllTracksCmd)
    fDatacardManager->SetSaveAllTracks(fKineSaveAllTracksCmd->GetNewBoolValue(val));
  else if (cmd==fKineBeamParticleEndProcessToRejectCmd)
    fDatacardManager->AddBeamParticleEndProcessToReject(val);
  else if (cmd==fKineZMinCmd)
    fDatacardManager->SetKineZMin(fKineZMinCmd->GetNewDoubleValue(val));
  else if (cmd==fKineZMaxCmd)
    fDatacardManager->SetKineZMax(fKineZMaxCmd->GetNewDoubleValue(val));
  else if (cmd==fKineParticleTypeToSaveCmd)
    fDatacardManager->AddParticleTypeToSave(val);
  else if (cmd==fKineCreatorProcessToSaveCmd)
    fDatacardManager->AddCreatorProcessToSave(val);
  else if (cmd==fKineEndProcessToSaveCmd)
    fDatacardManager->AddEndProcessToSave(val);
  else if (cmd==fKineParticleTypeToRejectCmd)
    fDatacardManager->AddParticleTypeToReject(val);
  else if (cmd==fKineCreatorProcessToRejectCmd)
    fDatacardManager->AddCreatorProcessToReject(val);
  else if (cmd==fKineEndProcessToRejectCmd)
    fDatacardManager->AddEndProcessToReject(val);
  else if (cmd==fKineMinECmd)
    fDatacardManager->SetKineEMin(fKineMinECmd->GetNewDoubleValue(val));
  else if (cmd==fKineMaxIntLevelCmd)
    fDatacardManager->SetMaxIntLevel(fKineMaxIntLevelCmd->GetNewIntValue(val));
  else if (cmd==fKineSaveVerboseCmd)
    fDatacardManager->SetSaveVerbose(fKineSaveVerboseCmd->GetNewBoolValue(val));
  else if (cmd==fFastSimuSchemeCmd)
    fDatacardManager->SetFastSimuSchemeStep(fFastSimuSchemeCmd->GetNewIntValue(val));
  else if (cmd==fFastSimuSchemeInputFileCmd)
    fDatacardManager->SetFastSimuSchemeInput(val);  
  else if (cmd==fFastSimulationModeCmd)
    fDatacardManager->SetFastSimulationMode(fFastSimulationModeCmd->GetNewIntValue(val));

  /////////////////////////////////////////////////////////
  // Sanity checks: run number and fiducial decay region.
  // They are executed much faster than those in RunAction.

  G4bool ParametersAreValid = kTRUE;

  if (DatacardManager::GetInstance()->GetDecayZmin()>=
      DatacardManager::GetInstance()->GetDecayZmax()) {
    G4cout << "[DatacardMessenger] Error: Decay volume Z(min) > Z(max)" << G4endl;
    ParametersAreValid = kFALSE;
  }
  if (!ParametersAreValid) exit(kWrongConfiguration);
}
