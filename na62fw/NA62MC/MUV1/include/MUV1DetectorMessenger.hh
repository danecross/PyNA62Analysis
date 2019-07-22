//
// --------------------------------------------------------------
// History:
//
// 2011-01-12 Created by Monica Pepe (monica.pepe@pg.infn.it) after
//   LAVDetectorMessenger by Emanuele Leonardi 
//
// 2011-06-22 Adapted for MUV1 by Mario Vormstein (mario.vormstein@uni-mainz.de)
//
//   - Define commands to en-/dis-able MUV1 Fast simulation
//
// --------------------------------------------------------------

#ifndef MUV1DetectorMessenger_h
#define MUV1DetectorMessenger_h 1

#include "globals.hh"
#include "G4UImessenger.hh"

class MUV1Detector;
class G4UIdirectory;
class G4UIcommand;
class G4UIcmdWithAString;
class G4UIcmdWithADouble;
class G4UIcmdWithAnInteger;

class MUV1DetectorMessenger: public G4UImessenger
{
public:
  explicit MUV1DetectorMessenger(MUV1Detector* );
  ~MUV1DetectorMessenger();

  void SetNewValue(G4UIcommand*, G4String);

private:

  MUV1Detector* fMUV1Detector;

  G4UIdirectory* fMUV1DetectorDir;

  G4UIcmdWithAString* fEnableFastSimulationCmd;

  G4UIcmdWithADouble* fHitContainerTimeLimitCmd;
  G4UIcmdWithAnInteger* fHitContainerScintillatorSegmentationCmd;

};
#endif
