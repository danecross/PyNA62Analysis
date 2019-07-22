// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-07-01
// ---------------------------------------------------------------------

/// \class CedarDetectorMessenger
/// \Brief
/// Reads Cedar-related settings from the macro file
/// \EndBrief

#include "CedarDetectorMessenger.hh"

#include "CedarDetector.hh"
#include "G4UIdirectory.hh"
#include "G4UIcommand.hh"
#include "G4UIparameter.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithoutParameter.hh"

CedarDetectorMessenger::CedarDetectorMessenger(CedarDetector* Det)
: fCedarDetector(Det) { 

  fCedarDetectorDir = new G4UIdirectory("/Detector/Cedar/");
  fCedarDetectorDir->SetGuidance("UI commands for Cedar detector simulation");
 
  fEnableCherenkovEffectCmd = new
    G4UIcmdWithAString("/Detector/Cedar/EnableCherenkovEffect",this);
  fEnableCherenkovEffectCmd->SetGuidance("Enable Cerenkov effect in the Cedar");
  fEnableCherenkovEffectCmd->SetParameterName("CedarCherenkovEffect",false);
  fEnableCherenkovEffectCmd->SetCandidates("false true");
}

CedarDetectorMessenger::~CedarDetectorMessenger() {
  delete fCedarDetectorDir;
  delete fEnableCherenkovEffectCmd;
}

void CedarDetectorMessenger::SetNewValue(G4UIcommand* cmd, G4String par) {
  if (cmd == fEnableCherenkovEffectCmd) {
    G4bool value = (par=="true") ? true : false;
    fCedarDetector->SetCherenkovEffect(value);
  }
}
