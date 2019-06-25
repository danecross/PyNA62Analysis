// LAVDetectorMessenger.hh
// --------------------------------------------------------------
// History:
//
// 2010-11-09 Created by Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - First implementation of class
//   - Define commands to en-/dis-able station
// 2010-11-23 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Define commands to en-/dis-able station, layer, banana, vessel
//   - Define commands to select block simulation type
// 2011-01-24 Domenico Di Filippo (difilippo@na.infn.it)
//   - Define commands to select the matrix file path
//
// --------------------------------------------------------------

#ifndef LAVDetectorMessenger_h
#define LAVDetectorMessenger_h 1

#include "globals.hh"
#include "G4UImessenger.hh"

class LAVDetector;
class G4UIdirectory;
class G4UIcommand;
class G4UIcmdWithAString;
class G4UIcmdWithoutParameter;

class LAVDetectorMessenger: public G4UImessenger
{
public:
  explicit LAVDetectorMessenger(LAVDetector* );
  ~LAVDetectorMessenger();

  void SetNewValue(G4UIcommand*, G4String);

private:

  LAVDetector* fLAVDetector;

  G4UIdirectory* fLAVDetectorDir;

  G4UIcmdWithoutParameter* fEnableAllStationsCmd;
  G4UIcmdWithoutParameter* fDisableAllStationsCmd;
  G4UIcmdWithoutParameter* fEnableAllLayersCmd;
  G4UIcmdWithoutParameter* fDisableAllLayersCmd;
  G4UIcmdWithoutParameter* fEnableAllBananasCmd;
  G4UIcmdWithoutParameter* fDisableAllBananasCmd;
  G4UIcmdWithoutParameter* fEnableAllVesselsCmd;
  G4UIcmdWithoutParameter* fDisableAllVesselsCmd;

  G4UIcommand* fEnableStationCmd;
  G4UIcommand* fDisableStationCmd;
  G4UIcommand* fEnableLayerCmd;
  G4UIcommand* fDisableLayerCmd;
  G4UIcommand* fEnableBananaCmd;
  G4UIcommand* fDisableBananaCmd;
  G4UIcommand* fEnableVesselCmd;
  G4UIcommand* fDisableVesselCmd;
  
  G4UIcmdWithAString* fSelectBlockSimulationCmd;
  G4UIcmdWithAString* fEfficiencyMatrixCmd;
  G4UIcmdWithAString* fTimeMatrixCmd;
};
#endif
