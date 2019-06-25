// CHANTIDetectorMessenger.cc
// --------------------------------------------------------------
// History:
//
// 2011-06-15 Created by Paolo MassarottiEmanuele & Domenicco Di Filippo
//   - First implementation of class
//   - Define commands to en-/dis-able Silicon Ring
//
// --------------------------------------------------------------

#include "CHANTIDetectorMessenger.hh"
#include "CHANTIDetector.hh"
#include "CHANTIGeometryParameters.hh"
#include "G4UIdirectory.hh"
#include "G4UIcommand.hh"
#include "G4UIparameter.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithoutParameter.hh"


CHANTIDetectorMessenger::CHANTIDetectorMessenger(CHANTIDetector* Detector)
{ 
  fCHANTIDetector = Detector;
  fCHANTIGeometryParameters = CHANTIGeometryParameters::GetInstance();

  fCHANTIDetectorDir = new G4UIdirectory("/Detector/CHANTI/");
  fCHANTIDetectorDir->SetGuidance("UI commands for CHANTI detector geometry");

  fEnableSiliconRingCmd = new G4UIcmdWithoutParameter("/Detector/CHANTI/EnableSiliconRing",this);
  fEnableSiliconRingCmd->SetGuidance("Enable CHANTI Silicon Ring.");
  fEnableSiliconRingCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableSiliconRingCmd = new G4UIcmdWithoutParameter("/Detector/CHANTI/DisableSiliconRing",this);
  fDisableSiliconRingCmd->SetGuidance("Disable CHANTI Silicon Ring.");
  fDisableSiliconRingCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fEnableVesselCmd = new G4UIcmdWithoutParameter("/Detector/CHANTI/EnableVessel",this);
  fEnableVesselCmd->SetGuidance("Enable CHANTI vessel.");
  fEnableVesselCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableVesselCmd = new G4UIcmdWithoutParameter("/Detector/CHANTI/DisableVessel",this);
  fDisableVesselCmd->SetGuidance("Disable CHANTI vessel.");
  fDisableVesselCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

}

CHANTIDetectorMessenger::~CHANTIDetectorMessenger()
{

  delete fCHANTIDetectorDir;

  delete fEnableSiliconRingCmd;
  delete fDisableSiliconRingCmd;
  delete fEnableVesselCmd;
  delete fDisableVesselCmd;
}

void CHANTIDetectorMessenger::SetNewValue(G4UIcommand* cmd, G4String)
{

 
 
  if( cmd == fEnableSiliconRingCmd ) {
    fCHANTIDetector->SetActiveSilRing(1);
    fCHANTIGeometryParameters->SetCHANTIVesselPatchLength(90.*mm);
  }
  if( cmd == fDisableSiliconRingCmd ) 
    {
      fCHANTIDetector->SetActiveSilRing(0);
      fCHANTIGeometryParameters->SetCHANTIVesselPatchLength(110.*mm);
    }

  if( cmd == fEnableVesselCmd )    fCHANTIDetector->SetVesselStatus(1);

  if( cmd == fDisableVesselCmd )   fCHANTIDetector->SetVesselStatus(0);
}
