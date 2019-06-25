//
// --------------------------------------------------------------
// History:
//
// 2011-01-12 Created by Monica Pepe (monica.pepe@pg.infn.it) after
//   LAVDetectorMessenger by Emanuele Leonardi 
//
//   - Define commands to en-/dis-able RICH Fast simulation
//
// --------------------------------------------------------------

#include "RICHDetectorMessenger.hh"

#include "RICHDetector.hh"
#include "G4UIdirectory.hh"
#include "G4UIcommand.hh"
#include "G4UIparameter.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithoutParameter.hh"

RICHDetectorMessenger::RICHDetectorMessenger(RICHDetector* Det)
:fRICHDetector(Det)
{ 

  fRICHDetectorDir = new G4UIdirectory("/Detector/RICH/");
  fRICHDetectorDir->SetGuidance("UI commands for RICH detector simulation");
}

RICHDetectorMessenger::~RICHDetectorMessenger()
{

  delete fRICHDetectorDir;

}

void RICHDetectorMessenger::SetNewValue(G4UIcommand*, G4String)
{
}
