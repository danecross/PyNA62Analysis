#ifndef LKrDetectorMessenger_h
#define LKrDetectorMessenger_h 1

#include "globals.hh"
#include "G4UImessenger.hh"

class LKrDetector;
class G4UIdirectory;
class G4UIcommand;
class G4UIcmdWithAString;
class G4UIcmdWithoutParameter;

class LKrDetectorMessenger: public G4UImessenger
{
public:
  explicit LKrDetectorMessenger(LKrDetector* );
  ~LKrDetectorMessenger();

  void SetNewValue(G4UIcommand*, G4String);

private:

  LKrDetector* fLKrDetector;

  G4UIdirectory* fLKrDetectorDir;

  G4UIcmdWithAString* fGeVtoCurrent1Cmd;
  G4UIcmdWithAString* fGeVtoCurrent2Cmd;
  G4UIcmdWithAString* fGeVtoCurrent3Cmd;

};
#endif
