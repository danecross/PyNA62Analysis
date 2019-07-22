//
// --------------------------------------------------------------
// History:
//
//   - 2018-02-14 Created by Gia.Khoriauli@cern.ch
//
//   - Define commands to configure a granularity of MUV2 readout
//
// --------------------------------------------------------------

#ifndef MUV2DetectorMessenger_h
#define MUV2DetectorMessenger_h 1

#include "globals.hh"
#include "G4UImessenger.hh"

class MUV2Detector;
class G4UIdirectory;
class G4UIcommand;
class G4UIcmdWithADouble;
class G4UIcmdWithAnInteger;

class MUV2DetectorMessenger: public G4UImessenger
{
public:
  explicit MUV2DetectorMessenger(MUV2Detector* );
  ~MUV2DetectorMessenger();

  void SetNewValue(G4UIcommand*, G4String);

private:

  MUV2Detector* fMUV2Detector;

  G4UIdirectory* fMUV2DetectorDir;

  G4UIcmdWithADouble* fHitContainerTimeLimitCmd;
  G4UIcmdWithAnInteger* fHitContainerScintillatorSegmentationCmd;

};
#endif
