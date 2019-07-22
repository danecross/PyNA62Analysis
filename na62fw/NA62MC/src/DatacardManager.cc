#include "DatacardManager.hh"
#include "G4RunManager.hh"
#include "G4Event.hh"
#include "G4Run.hh"
#include "DatacardMessenger.hh"
#include "G4UImanager.hh"
#include "DetectorMessenger.hh"
#include "NA62ConditionsService.hh"

DatacardManager* DatacardManager::fInstance = nullptr;

DatacardManager::DatacardManager() :
  datacardMessenger(nullptr), fOutputFileName("default.root"),
  fDecayForce(true), fMuonDecayForce(false), fPionDecayForce(0),
  fDecayType(0), fDecayRadcor(0), fDecayPizeroDecay(0),
  fTwoPhotonsMaxAngle(-1), fRadiativePhotonMinEnergy(-1), fRadiativePhotonMaxEnergy(-1), fLeptonPhotonMinAngle(-1),
  fMinTracksMomentum(0), fDecayZMin(102425.), fDecayZMax(180000.), fDecayVerbose(0),
  fRandDecaySeed(10), fRunNumber(0), fFastSimuSchemeStep(0), fFastSimulationMode(0) {}

DatacardManager::~DatacardManager() {
  delete datacardMessenger;
}

DatacardManager* DatacardManager::GetInstance() {
  if (!fInstance) fInstance = new DatacardManager();
  return fInstance;
}

void DatacardManager::SetMessenger() {
  datacardMessenger = new DatacardMessenger(this);
}

void DatacardManager::SetFastSimuSchemeInput(G4String value) {
  G4UImanager* UImanager = G4UImanager::GetUIpointer();
  G4String cmd = "/beam/ReplayFile " + value;
  UImanager->ApplyCommand(cmd);
}

void DatacardManager::SetFastSimuSchemeStep(G4int value) {
  fFastSimuSchemeStep = value;
  G4UImanager* UImanager = G4UImanager::GetUIpointer();

  if (value==1) {
    SetSaveTracksForRegeneration(true);
    UImanager->ApplyCommand("/Detector/DisableSubDetector Cedar");
    UImanager->ApplyCommand("/Detector/DisableSubDetector CHANTI");
    UImanager->ApplyCommand("/Detector/DisableSubDetector CHOD");
    UImanager->ApplyCommand("/Detector/DisableSubDetector GigaTracker");
    UImanager->ApplyCommand("/Detector/DisableSubDetector HAC");
    UImanager->ApplyCommand("/Detector/DisableSubDetector IRC");
    UImanager->ApplyCommand("/Detector/DisableSubDetector LAV");
    UImanager->ApplyCommand("/Detector/DisableSubDetector LKr");
    UImanager->ApplyCommand("/Detector/DisableSubDetector MUV0");
    UImanager->ApplyCommand("/Detector/DisableSubDetector MUV1");
    UImanager->ApplyCommand("/Detector/DisableSubDetector MUV2");
    UImanager->ApplyCommand("/Detector/DisableSubDetector MUV3");
    UImanager->ApplyCommand("/Detector/DisableSubDetector NewCHOD");
    UImanager->ApplyCommand("/Detector/DisableSubDetector RICH");
    UImanager->ApplyCommand("/Detector/DisableSubDetector SAC");
    UImanager->ApplyCommand("/Detector/DisableSubDetector Spectrometer");
    UImanager->ApplyCommand("/Detector/EnableSubDetector Cedar");
    UImanager->ApplyCommand("/Detector/EnableSubDetector GigaTracker");
    UImanager->ApplyCommand("/Detector/EnableSubDetector Spectrometer");
  }
  else if (value==2) {
    UImanager->ApplyCommand("/beam/SetBeam mcreplay");
    UImanager->ApplyCommand("/Detector/DisableSubDetector Cedar");
    UImanager->ApplyCommand("/Detector/DisableSubDetector CHANTI");
    UImanager->ApplyCommand("/Detector/DisableSubDetector CHOD");
    UImanager->ApplyCommand("/Detector/DisableSubDetector GigaTracker");
    UImanager->ApplyCommand("/Detector/DisableSubDetector HAC");
    UImanager->ApplyCommand("/Detector/DisableSubDetector IRC");
    UImanager->ApplyCommand("/Detector/DisableSubDetector LAV");
    UImanager->ApplyCommand("/Detector/DisableSubDetector LKr");
    UImanager->ApplyCommand("/Detector/DisableSubDetector MUV0");
    UImanager->ApplyCommand("/Detector/DisableSubDetector MUV1");
    UImanager->ApplyCommand("/Detector/DisableSubDetector MUV2");
    UImanager->ApplyCommand("/Detector/DisableSubDetector MUV3");
    UImanager->ApplyCommand("/Detector/DisableSubDetector NewCHOD");
    UImanager->ApplyCommand("/Detector/DisableSubDetector RICH");
    UImanager->ApplyCommand("/Detector/DisableSubDetector SAC");
    UImanager->ApplyCommand("/Detector/DisableSubDetector Spectrometer");
    UImanager->ApplyCommand("/Detector/EnableSubDetector CHANTI");
    UImanager->ApplyCommand("/Detector/EnableSubDetector CHOD");
    UImanager->ApplyCommand("/Detector/EnableSubDetector HAC");
    UImanager->ApplyCommand("/Detector/EnableSubDetector IRC");
    UImanager->ApplyCommand("/Detector/EnableSubDetector LAV");
    UImanager->ApplyCommand("/Detector/EnableSubDetector LKr");
    UImanager->ApplyCommand("/Detector/EnableSubDetector MUV0");
    UImanager->ApplyCommand("/Detector/EnableSubDetector MUV1");
    UImanager->ApplyCommand("/Detector/EnableSubDetector MUV2");
    UImanager->ApplyCommand("/Detector/EnableSubDetector MUV3");
    UImanager->ApplyCommand("/Detector/EnableSubDetector RICH");
    UImanager->ApplyCommand("/Detector/EnableSubDetector SAC");
  }
}

void DatacardManager::SetFastSimulationMode(G4int value) {
  fFastSimulationMode = value;
  if (!value) return; // fast MC not requested
  G4cout <<"[DatacardManager] Fast simulation requested: mode " << value << G4endl;

  G4UImanager* UImanager = G4UImanager::GetUIpointer();
  UImanager->ApplyCommand("/Detector/EnableSubDetector Cedar");
  UImanager->ApplyCommand("/Detector/EnableSubDetector GigaTracker");
  UImanager->ApplyCommand("/Detector/EnableSubDetector CHANTI");
  UImanager->ApplyCommand("/Detector/EnableSubDetector LAV");
  UImanager->ApplyCommand("/Detector/EnableSubDetector Spectrometer");
  UImanager->ApplyCommand("/Detector/Cedar/EnableCherenkovEffect false");
  if (value==1) { // normal fast MC
    UImanager->ApplyCommand("/Detector/DisableSubDetector RICH");
  }
  else if (value==2) { // fast MC with RICH
    UImanager->ApplyCommand("/Detector/EnableSubDetector RICH");
  }
  UImanager->ApplyCommand("/Detector/DisableSubDetector NewCHOD");
  UImanager->ApplyCommand("/Detector/LAV/DisableStation 11"); // disable LAV12 
  UImanager->ApplyCommand("/Detector/DisableSubDetector CHOD");
  UImanager->ApplyCommand("/Detector/DisableSubDetector LKr");
  UImanager->ApplyCommand("/Detector/DisableSubDetector IRC");
  UImanager->ApplyCommand("/Detector/DisableSubDetector SAC");
  UImanager->ApplyCommand("/Detector/DisableSubDetector HAC");
  UImanager->ApplyCommand("/Detector/DisableSubDetector MUV0");
  UImanager->ApplyCommand("/Detector/DisableSubDetector MUV1");
  UImanager->ApplyCommand("/Detector/DisableSubDetector MUV2");
  UImanager->ApplyCommand("/Detector/DisableSubDetector MUV3");
}

/////////////////////////////////
// Turtle beam datacard file name

TString DatacardManager::GetTurtleDatacardFileName() {
  if      (fRunNumber>=1560 && fRunNumber<=4173)
    return NA62ConditionsService::GetInstance()->GetFullPath("turtle2015.dat");
  else if (fRunNumber>=4174 && fRunNumber<=6941)
    return NA62ConditionsService::GetInstance()->GetFullPath("turtle2016.dat");
  else if (fRunNumber>=6942 && fRunNumber<=8306)
    return NA62ConditionsService::GetInstance()->GetFullPath("turtle2017.dat");
  else if (fRunNumber>=8307 && fRunNumber<=9463)
    return NA62ConditionsService::GetInstance()->GetFullPath("turtle2018.dat");
  else
    return NA62ConditionsService::GetInstance()->GetFullPath("turtle_default.dat"); // includes RunNumber=0
}

void DatacardManager::SetRunNumber(G4int val) {
  fRunNumber = val;
}

Double_t DatacardManager::GetBeamAlignmentX() { // [mm]
  if      (fRunNumber>=1560 && fRunNumber<=4173) return  0.2; // 2015 conditions
  else if (fRunNumber>=4174 && fRunNumber<=6941) return -0.6; // 2016 conditions
  else if (fRunNumber>=6942 && fRunNumber<=8306) return -1.5; // 2017 conditions
  else if (fRunNumber>=8307 && fRunNumber<=9463) return  0.0; // 2018 conditions
  return 0.0;
}

Double_t DatacardManager::GetBeamAlignmentY() { // [mm]
  if      (fRunNumber>=1560 && fRunNumber<=4173) return -1.3; // 2015 conditions
  else if (fRunNumber>=4174 && fRunNumber<=6941) return -1.3; // 2016 conditions
  else if (fRunNumber>=6942 && fRunNumber<=8306) return -1.4; // 2017 conditions
  else if (fRunNumber>=8307 && fRunNumber<=9463) return  0.0; // 2018 conditions
  return 0.0;
}

Double_t DatacardManager::GetBeamMomentumScaleFactor() {
  if      (fRunNumber>=1560 && fRunNumber<=4173) return 0.9990; // 2015 conditions
  else if (fRunNumber>=4174 && fRunNumber<=6941) return 1.0000; // 2016 conditions
  else if (fRunNumber>=6942 && fRunNumber<=8306) return 0.9978; // 2017 conditions
  else if (fRunNumber>=8307 && fRunNumber<=9463) return 1.0000; // 2018 conditions
  return 1.000;
}

/////////////////////////////////////////////////////////////////////////////
// Bend6 field scale factor determines the beam dy/dz
// The sensitivity is d(dy/dz) / d(ScaleFactor) = -0.0165.
// So increasing the scale factor by 0.0001 decreases dy/dz by 1.65 microrad.

Double_t DatacardManager::GetBend6FieldScaleFactor() {
  if      (fRunNumber>=1560 && fRunNumber<=4173) return 1.0010; // 2015 conditions
  else if (fRunNumber>=4174 && fRunNumber<=6941) return 0.9980; // 2016 conditions
  else if (fRunNumber>=6942 && fRunNumber<=8306) return 0.9992; // 2017 conditions
  else if (fRunNumber>=8307 && fRunNumber<=9463) return 1.0000; // 2018 conditions
  return 1.0000;
}

/////////////////////////////////////////////////////////////////////////////
// Trim5 field scale factor determines the beam dx/dz
// The sensitivity is d(dx/dz) / d(ScaleFactor) = 0.0012.
// So increasing the scale factor by 0.001 increases dx/dz by 1.2 microrad.

Double_t DatacardManager::GetTrim5FieldScaleFactor() {
  if      (fRunNumber>=1560 && fRunNumber<=4173) return 1.000; // 2015 conditions
  else if (fRunNumber>=4174 && fRunNumber<=6941) return 1.024; // 2016 conditions
  else if (fRunNumber>=6942 && fRunNumber<=8306) return 1.001; // 2017 conditions
  else if (fRunNumber>=8307 && fRunNumber<=9463) return 1.000; // 2018 conditions
  return 1.0000;
}

Double_t DatacardManager::GetQuad09FieldScaleFactor() {
  if      (fRunNumber>=1560 && fRunNumber<=4173) return 0.95; // 2015 conditions
  else if (fRunNumber>=4174 && fRunNumber<=6941) return 1.00; // 2016 conditions
  else if (fRunNumber>=6942 && fRunNumber<=8306) return 0.99; // 2017 conditions
  else if (fRunNumber>=8307 && fRunNumber<=9463) return 1.00; // 2018 conditions
  return 1.00;
}

Double_t DatacardManager::GetQuad10FieldScaleFactor() {
  if      (fRunNumber>=1560 && fRunNumber<=4173) return 1.03; // 2015 conditions
  else if (fRunNumber>=4174 && fRunNumber<=6941) return 1.00; // 2016 conditions
  else if (fRunNumber>=6942 && fRunNumber<=8306) return 1.00; // 2017 conditions
  else if (fRunNumber>=8307 && fRunNumber<=9463) return 1.00; // 2018 conditions
  return 1.00;
}
