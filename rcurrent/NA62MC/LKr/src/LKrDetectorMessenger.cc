
#include "LKrDetectorMessenger.hh"

#include "LKrDetector.hh"
#include "G4UIdirectory.hh"
#include "G4UIcommand.hh"
#include "G4UIparameter.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithoutParameter.hh"

#include "LKrGeometryParameters.hh"
#include "StringInterpreter.hh"

LKrDetectorMessenger::LKrDetectorMessenger(LKrDetector* Det)
:fLKrDetector(Det)
{ 

  fLKrDetectorDir = new G4UIdirectory("/Detector/LKr/");
  fLKrDetectorDir->SetGuidance("UI commands for LKr detector geometry");

  fGeVtoCurrent1Cmd = new G4UIcmdWithAString("/Detector/LKr/GeVtoCurrent1",this);
  fGeVtoCurrent1Cmd->SetGuidance("Select path for GeV to Current conversion (1).");
  fGeVtoCurrent1Cmd->SetParameterName("FilePath",false);

  fGeVtoCurrent2Cmd = new G4UIcmdWithAString("/Detector/LKr/GeVtoCurrent2",this);
  fGeVtoCurrent2Cmd->SetGuidance("Select path for GeV to Current conversion (2).");
  fGeVtoCurrent2Cmd->SetParameterName("FilePath",false);

  fGeVtoCurrent3Cmd = new G4UIcmdWithAString("/Detector/LKr/GeVtoCurrent3",this);
  fGeVtoCurrent3Cmd->SetGuidance("Select path for GeV to Current conversion (3).");
  fGeVtoCurrent3Cmd->SetParameterName("FilePath",false);
}

LKrDetectorMessenger::~LKrDetectorMessenger()
{
  delete fGeVtoCurrent1Cmd;
  delete fGeVtoCurrent2Cmd;
  delete fGeVtoCurrent3Cmd;
}

void LKrDetectorMessenger::SetNewValue(G4UIcommand* cmd, G4String par)
{

  if ( cmd == fGeVtoCurrent1Cmd ) LKrGeometryParameters::GetInstance()->SetGeVtoCurrent1((G4String)StringInterpreter(par));
  if ( cmd == fGeVtoCurrent2Cmd ) LKrGeometryParameters::GetInstance()->SetGeVtoCurrent2((G4String)StringInterpreter(par));
  if ( cmd == fGeVtoCurrent3Cmd ) LKrGeometryParameters::GetInstance()->SetGeVtoCurrent3((G4String)StringInterpreter(par));

}
