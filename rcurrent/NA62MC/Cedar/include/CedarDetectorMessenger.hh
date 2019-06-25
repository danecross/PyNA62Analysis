// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-07-01
// ---------------------------------------------------------------------

#ifndef CedarDetectorMessenger_h
#define CedarDetectorMessenger_h 1

#include "globals.hh"
#include "G4UImessenger.hh"

class CedarDetector;
class G4UIdirectory;
class G4UIcommand;
class G4UIcmdWithAString;
class G4UIcmdWithoutParameter;

class CedarDetectorMessenger : public G4UImessenger {
public:
  explicit CedarDetectorMessenger(CedarDetector*);
  ~CedarDetectorMessenger();

  void SetNewValue(G4UIcommand*, G4String);

private:

  CedarDetector* fCedarDetector;
  G4UIdirectory* fCedarDetectorDir;
  G4UIcmdWithAString* fEnableCherenkovEffectCmd;

};
#endif
