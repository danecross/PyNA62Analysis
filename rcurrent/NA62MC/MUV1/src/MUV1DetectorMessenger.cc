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
// 2018-02-14 modified by Gia.Khoriauli@cern.ch
//
//   - Define commands to configure a granularity of MUV1 readout
//
// --------------------------------------------------------------

#include "MUV1DetectorMessenger.hh"
#include "MUV1GeometryParameters.hh"

#include "MUV1Detector.hh"
#include "G4UIdirectory.hh"
#include "G4UIcommand.hh"
#include "G4UIparameter.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithADouble.hh"
#include "G4UIcmdWithAnInteger.hh"

/// \class MUV1DetectorMessenger
/// \Brief
/// MUV1DetectorMessenger class.
/// \EndBrief
///
/// \Detailed
//  Messenger class of the MUV1
/// \EndDetailed

MUV1DetectorMessenger::MUV1DetectorMessenger(MUV1Detector* Det)
:fMUV1Detector(Det)
{ 

  fMUV1DetectorDir = new G4UIdirectory("/Detector/MUV1/");
  fMUV1DetectorDir->SetGuidance("UI commands for MUV1 detector simulation");

 
  fEnableFastSimulationCmd = new G4UIcmdWithAString("/Detector/MUV1/EnableFullOpticalSimulation",this);
  fEnableFastSimulationCmd->SetGuidance("Enable MUV1 full optical simulation");
  fEnableFastSimulationCmd->SetParameterName("FastSimulation",false);
  fEnableFastSimulationCmd->SetCandidates("false true");

  fHitContainerTimeLimitCmd = new G4UIcmdWithADouble("/Detector/MUV1/HitContainerTimeLimit",this);
  fHitContainerTimeLimitCmd->SetGuidance("Time window for combining G4 hits into MUV1 hit objects");
  fHitContainerTimeLimitCmd->SetParameterName("TimeLimit",false);
  fHitContainerTimeLimitCmd->SetDefaultValue(20);
  fHitContainerTimeLimitCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fHitContainerScintillatorSegmentationCmd = new G4UIcmdWithAnInteger("/Detector/MUV1/HitContainerScintillatorSegmentation",this);
  fHitContainerScintillatorSegmentationCmd->SetGuidance("Determines a space granularity of MUV1 scintillators for combining G4 hits into MUV1 hit objects");
  fHitContainerScintillatorSegmentationCmd->SetParameterName("ScintillatorSegmentation",false);
  fHitContainerScintillatorSegmentationCmd->SetDefaultValue(100);
  fHitContainerScintillatorSegmentationCmd->AvailableForStates(G4State_PreInit, G4State_Idle);
}

MUV1DetectorMessenger::~MUV1DetectorMessenger()
{

  delete fMUV1DetectorDir;

  delete fEnableFastSimulationCmd;

  delete fHitContainerTimeLimitCmd;
  delete fHitContainerScintillatorSegmentationCmd;

}

void MUV1DetectorMessenger::SetNewValue(G4UIcommand* cmd, G4String par)
{
  // Switch between full optical and fast simulation.
  // default: full optical simulation disabled
  if (cmd == fEnableFastSimulationCmd){
	  G4String  fastSimulation;
	if(par=="false") {					// So the fast simulation is enabled
		fastSimulation = "true";
	}
	else{
		fastSimulation = "false";
	}

    fMUV1Detector->SetFastSimulation(fastSimulation);
  }

  else if(cmd == fHitContainerTimeLimitCmd)	{
	  MUV1GeometryParameters::GetInstance()->SetHitContainerTimeLimit( fHitContainerTimeLimitCmd->GetNewDoubleValue(par) );
  }
  else if(cmd == fHitContainerScintillatorSegmentationCmd)	{

	  MUV1GeometryParameters::GetInstance()->SetHitContainerScintillatorSegmentation( fHitContainerScintillatorSegmentationCmd->GetNewIntValue(par) );
	  MUV1GeometryParameters::GetInstance()->SetHitContainerDimension( 2800 / fHitContainerScintillatorSegmentationCmd->GetNewIntValue(par) );
  }
    
}
