
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
:fLKrDetector(Det){
}

LKrDetectorMessenger::~LKrDetectorMessenger(){
}

