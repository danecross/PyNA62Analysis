//
// --------------------------------------------------------------
// History:
//
// 2018-02-14 Created by Gia.Khoriauli@cern.ch)
//
//   - Define commands to configure a granularity of MUV2 readout
//
// --------------------------------------------------------------

#include "MUV2DetectorMessenger.hh"
#include "MUV2GeometryParameters.hh"

#include "MUV2Detector.hh"
#include "G4UIdirectory.hh"
#include "G4UIcommand.hh"
#include "G4UIparameter.hh"
#include "G4UIcmdWithADouble.hh"
#include "G4UIcmdWithAnInteger.hh"

/// \class MUV2DetectorMessenger
/// \Brief
/// MUV2DetectorMessenger class.
/// \EndBrief
///
/// \Detailed
//  Messenger class of the MUV2
/// \EndDetailed

MUV2DetectorMessenger::MUV2DetectorMessenger(MUV2Detector* Det)
:fMUV2Detector(Det)
{

  fMUV2DetectorDir = new G4UIdirectory("/Detector/MUV2/");
  fMUV2DetectorDir->SetGuidance("UI commands for MUV2 detector simulation");

  fHitContainerTimeLimitCmd = new G4UIcmdWithADouble("/Detector/MUV2/HitContainerTimeLimit",this);
  fHitContainerTimeLimitCmd->SetGuidance("Time window for collecting G4 hits in MUV2 hit objects");
  fHitContainerTimeLimitCmd->SetParameterName("TimeLimit",false);
  fHitContainerTimeLimitCmd->SetDefaultValue(20);
  fHitContainerTimeLimitCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

  fHitContainerScintillatorSegmentationCmd = new G4UIcmdWithAnInteger("/Detector/MUV2/HitContainerScintillatorSegmentation",this);
  fHitContainerScintillatorSegmentationCmd->SetGuidance("Determines a space granularity of MUV2 scintillators for combining G4 hits into MUV2 hit objects");
  fHitContainerScintillatorSegmentationCmd->SetParameterName("ScintillatorSegmentation",false);
  fHitContainerScintillatorSegmentationCmd->SetDefaultValue(100);
  fHitContainerScintillatorSegmentationCmd->AvailableForStates(G4State_PreInit, G4State_Idle);

}

MUV2DetectorMessenger::~MUV2DetectorMessenger()
{

  delete fMUV2DetectorDir;

  delete fHitContainerTimeLimitCmd;
  delete fHitContainerScintillatorSegmentationCmd;

}

void MUV2DetectorMessenger::SetNewValue(G4UIcommand* cmd, G4String par)
{
  if(cmd == fHitContainerTimeLimitCmd)	{
	  MUV2GeometryParameters::GetInstance()->SetHitContainerTimeLimit( fHitContainerTimeLimitCmd->GetNewDoubleValue(par) );
  }
  else if(cmd == fHitContainerScintillatorSegmentationCmd)	{

	  MUV2GeometryParameters::GetInstance()->SetHitContainerScintillatorSegmentation( fHitContainerScintillatorSegmentationCmd->GetNewIntValue(par) );
	  MUV2GeometryParameters::GetInstance()->SetHitContainerDimension( 2800 / fHitContainerScintillatorSegmentationCmd->GetNewIntValue(par) );
  }

}
