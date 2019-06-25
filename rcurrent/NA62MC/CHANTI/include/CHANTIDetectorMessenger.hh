// CHANTIDetectorMessenger.hh
// --------------------------------------------------------------
// History:
//
// 2011-06-15 Created by Paolo MassarottiEmanuele & Domenicco Di Filippo
//   - First implementation of class
//   - Define commands to en-/dis-able Silicon Ring
//
// --------------------------------------------------------------

#ifndef CHANTIDetectorMessenger_h
#define CHANTIDetectorMessenger_h 1

#include "globals.hh"
#include "G4UImessenger.hh"

class CHANTIDetector;
class CHANTIGeometryParameters;
class G4UIdirectory;
class G4UIcommand;
class G4UIcmdWithAString;
class G4UIcmdWithoutParameter;

class CHANTIDetectorMessenger: public G4UImessenger
{
public:
  explicit CHANTIDetectorMessenger( CHANTIDetector* );
  virtual ~CHANTIDetectorMessenger();

  void SetNewValue(G4UIcommand*, G4String);

private:
  
  CHANTIGeometryParameters* fCHANTIGeometryParameters;

  G4UIdirectory* fCHANTIDetectorDir;

  G4UIcmdWithoutParameter* fEnableSiliconRingCmd;
  G4UIcmdWithoutParameter* fDisableSiliconRingCmd;
  G4UIcmdWithoutParameter* fEnableVesselCmd;
  G4UIcmdWithoutParameter* fDisableVesselCmd;
  CHANTIDetector* fCHANTIDetector; 

};
#endif
