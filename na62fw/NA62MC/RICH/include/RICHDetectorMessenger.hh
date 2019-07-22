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

#ifndef RICHDetectorMessenger_h
#define RICHDetectorMessenger_h 1

#include "globals.hh"
#include "G4UImessenger.hh"

class RICHDetector;
class G4UIdirectory;
class G4UIcommand;
class G4UIcmdWithAString;
class G4UIcmdWithoutParameter;

class RICHDetectorMessenger: public G4UImessenger
{
public:
  explicit RICHDetectorMessenger(RICHDetector* );
  ~RICHDetectorMessenger();

  void SetNewValue(G4UIcommand*, G4String);

private:

  RICHDetector* fRICHDetector;

  G4UIdirectory* fRICHDetectorDir;

};
#endif
