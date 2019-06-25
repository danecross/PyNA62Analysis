#ifndef DatacardMessenger_h
#define DatacardMessenger_h 1

#include "G4UImessenger.hh"
#include "globals.hh"

class DatacardManager;
class G4UIdirectory;
class G4UIcmdWithAString;
class G4UIcmdWithADouble;
class G4UIcmdWithAnInteger;
class G4UIcmdWithABool;
class G4UIcmdWith3Vector;

class DatacardMessenger: public G4UImessenger {

public:
  explicit DatacardMessenger(DatacardManager*);
  ~DatacardMessenger();
  void SetNewValue(G4UIcommand*, G4String);

private:
  DatacardManager*      fDatacardManager;

  G4UIcmdWithAString*   fOutputFileNameCmd;
  G4UIcmdWithABool*     fDecayForceCmd;
  G4UIcmdWithABool*     fMuonDecayForceCmd;
  G4UIcmdWithAnInteger* fPionDecayForceCmd;
  G4UIcmdWithAnInteger* fDecayTypeCmd;
  G4UIcmdWithAnInteger* fDecayRadcorCmd;
  G4UIcmdWith3Vector*   fDecayPizeroDecayCmd;
  G4UIcmdWithADouble*   fTwoPhotonsMaxAngle;
  G4UIcmdWithADouble*   fRadiativePhotonMinEnergy;
  G4UIcmdWithADouble*   fRadiativePhotonMaxEnergy;
  G4UIcmdWithADouble*   fLeptonPhotonMinAngle;
  G4UIcmdWithADouble*   fMinTracksMomentum;
  G4UIcmdWithAString*   fKineBeamParticleEndProcessToRejectCmd;
  G4UIcmdWithADouble*   fDecayZMinCmd;
  G4UIcmdWithADouble*   fDecayZMaxCmd;
  G4UIcmdWithAnInteger* fDecayVerboseCmd;
  G4UIcmdWithADouble*   fRandomSeedCmd;  // unsigned int get overflow with G4UIcmdWithAnInteger

  G4UIcmdWithABool*     fKineSaveAllStepsCmd;
  G4UIcmdWithABool*     fKineSaveAllTracksCmd;
  G4UIcmdWithADouble*   fKineZMinCmd;
  G4UIcmdWithADouble*   fKineZMaxCmd;
  G4UIcmdWithAString*   fKineParticleTypeToSaveCmd;
  G4UIcmdWithAString*   fKineCreatorProcessToSaveCmd;
  G4UIcmdWithAString*   fKineEndProcessToSaveCmd;
  G4UIcmdWithAString*   fKineParticleTypeToRejectCmd;
  G4UIcmdWithAString*   fKineCreatorProcessToRejectCmd;
  G4UIcmdWithAString*   fKineEndProcessToRejectCmd;
  G4UIcmdWithADouble*   fKineMinECmd;
  G4UIcmdWithAnInteger* fKineMaxIntLevelCmd;
  G4UIcmdWithABool*     fKineSaveVerboseCmd;
  G4UIcmdWithAnInteger* fFastSimuSchemeCmd;
  G4UIcmdWithAString*   fFastSimuSchemeInputFileCmd;
  G4UIcmdWithAnInteger* fFastSimulationModeCmd;
};

#endif
