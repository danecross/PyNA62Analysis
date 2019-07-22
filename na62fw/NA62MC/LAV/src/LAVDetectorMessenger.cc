// LAVDetectorMessenger.cc
// --------------------------------------------------------------
// History:
//
// 2010-11-09 Created by Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - First implementation of class
//   - Define commands to en-/dis-able station
// 2010-11-23 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Define commands to en-/dis-able layer, banana, vessel
//   - Define commands to select block simulation type
// 2011-01-24 Domenico Di Filippo (difilippo@na.infn.it)
//   - Define commands to select the matrix file path
//
// --------------------------------------------------------------

#include "LAVDetectorMessenger.hh"

#include "LAVDetector.hh"
#include "G4UIdirectory.hh"
#include "G4UIcommand.hh"
#include "G4UIparameter.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithoutParameter.hh"

#include "LAVGeometryParameters.hh"

LAVDetectorMessenger::LAVDetectorMessenger(LAVDetector* Det)
:fLAVDetector(Det)
{ 

  fLAVDetectorDir = new G4UIdirectory("/Detector/LAV/");
  fLAVDetectorDir->SetGuidance("UI commands for LAV detector geometry");

  fEnableAllStationsCmd = new G4UIcmdWithoutParameter("/Detector/LAV/EnableAllStations",this);
  fEnableAllStationsCmd->SetGuidance("Enable all LAV stations.");
  fEnableAllStationsCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableAllStationsCmd = new G4UIcmdWithoutParameter("/Detector/LAV/DisableAllStations",this);
  fDisableAllStationsCmd->SetGuidance("Disable all LAV stations.");
  fDisableAllStationsCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fEnableAllLayersCmd = new G4UIcmdWithoutParameter("/Detector/LAV/EnableAllLayers",this);
  fEnableAllLayersCmd->SetGuidance("Enable all LAV layers.");
  fEnableAllLayersCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableAllLayersCmd = new G4UIcmdWithoutParameter("/Detector/LAV/DisableAllLayers",this);
  fDisableAllLayersCmd->SetGuidance("Disable all LAV layers.");
  fDisableAllLayersCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fEnableAllBananasCmd = new G4UIcmdWithoutParameter("/Detector/LAV/EnableAllBananas",this);
  fEnableAllBananasCmd->SetGuidance("Enable all LAV bananas.");
  fEnableAllBananasCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableAllBananasCmd = new G4UIcmdWithoutParameter("/Detector/LAV/DisableAllBananas",this);
  fDisableAllBananasCmd->SetGuidance("Disable all LAV bananas.");
  fDisableAllBananasCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fEnableAllVesselsCmd = new G4UIcmdWithoutParameter("/Detector/LAV/EnableAllVessels",this);
  fEnableAllVesselsCmd->SetGuidance("Enable all LAV vessels.");
  fEnableAllVesselsCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableAllVesselsCmd = new G4UIcmdWithoutParameter("/Detector/LAV/DisableAllVessels",this);
  fDisableAllVesselsCmd->SetGuidance("Disable all LAV vessels.");
  fDisableAllVesselsCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fEnableStationCmd = new G4UIcommand("/Detector/LAV/EnableStation",this);
  fEnableStationCmd->SetGuidance("Enable a LAV station.");
  G4UIparameter* esStationParameter = new G4UIparameter("Station",'i',false);
  esStationParameter->SetParameterRange("Station >= 0 && Station <= 11");
  fEnableStationCmd->SetParameter(esStationParameter);
  fEnableStationCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableStationCmd = new G4UIcommand("/Detector/LAV/DisableStation",this);
  fDisableStationCmd->SetGuidance("Disable a LAV station.");
  G4UIparameter* dsStationParameter = new G4UIparameter("Station",'i',false);
  dsStationParameter->SetParameterRange("Station >= 0 && Station <= 11");
  fDisableStationCmd->SetParameter(dsStationParameter);
  fDisableStationCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fEnableLayerCmd = new G4UIcommand("/Detector/LAV/EnableLayer",this);
  fEnableLayerCmd->SetGuidance("Enable a LAV layer.");
  G4UIparameter* elStationParameter = new G4UIparameter("Station",'i',false);
  elStationParameter->SetParameterRange("Station >= 0 && Station <= 11");
  fEnableLayerCmd->SetParameter(elStationParameter);
  G4UIparameter* elLayerParameter = new G4UIparameter("Layer",'i',false);
  elLayerParameter->SetParameterRange("Layer >= 0 && Layer <= 4");
  fEnableLayerCmd->SetParameter(elLayerParameter);
  fEnableLayerCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableLayerCmd = new G4UIcommand("/Detector/LAV/DisableLayer",this);
  fDisableLayerCmd->SetGuidance("Disable a LAV layer.");
  G4UIparameter* dlStationParameter = new G4UIparameter("Station",'i',false);
  dlStationParameter->SetParameterRange("Station >= 0 && Station <= 11");
  fDisableLayerCmd->SetParameter(dlStationParameter);
  G4UIparameter* dlLayerParameter = new G4UIparameter("Layer",'i',false);
  dlLayerParameter->SetParameterRange("Layer >= 0 && Layer <= 4");
  fDisableLayerCmd->SetParameter(dlLayerParameter);
  fDisableLayerCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fEnableBananaCmd = new G4UIcommand("/Detector/LAV/EnableBanana",this);
  fEnableBananaCmd->SetGuidance("Enable a LAV banana.");
  G4UIparameter* ebStationParameter = new G4UIparameter("Station",'i',false);
  ebStationParameter->SetParameterRange("Station >= 0 && Station <= 11");
  fEnableBananaCmd->SetParameter(ebStationParameter);
  G4UIparameter* ebLayerParameter = new G4UIparameter("Layer",'i',false);
  ebLayerParameter->SetParameterRange("Layer >= 0 && Layer <= 4");
  fEnableBananaCmd->SetParameter(ebLayerParameter);
  G4UIparameter* ebBananaParameter = new G4UIparameter("Banana",'i',false);
  ebBananaParameter->SetParameterRange("Banana >= 0 && Banana <= 15");
  fEnableBananaCmd->SetParameter(ebBananaParameter);
  fEnableBananaCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableBananaCmd = new G4UIcommand("/Detector/LAV/DisableBanana",this);
  fDisableBananaCmd->SetGuidance("Disable a LAV banana.");
  G4UIparameter* dbStationParameter = new G4UIparameter("Station",'i',false);
  dbStationParameter->SetParameterRange("Station >= 0 && Station <= 11");
  fDisableBananaCmd->SetParameter(dbStationParameter);
  G4UIparameter* dbLayerParameter = new G4UIparameter("Layer",'i',false);
  dbLayerParameter->SetParameterRange("Layer >= 0 && Layer <= 4");
  fDisableBananaCmd->SetParameter(dbLayerParameter);
  G4UIparameter* dbBananaParameter = new G4UIparameter("Banana",'i',false);
  dbBananaParameter->SetParameterRange("Banana >= 0 && Banana <= 15");
  fDisableBananaCmd->SetParameter(dbBananaParameter);
  fDisableBananaCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fEnableVesselCmd = new G4UIcommand("/Detector/LAV/EnableVessel",this);
  fEnableVesselCmd->SetGuidance("Enable a LAV vessel.");
  G4UIparameter* evStationParameter = new G4UIparameter("Station",'i',false);
  evStationParameter->SetParameterRange("Station >= 0 && Station <= 11");
  fEnableVesselCmd->SetParameter(evStationParameter);
  fEnableVesselCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableVesselCmd = new G4UIcommand("/Detector/LAV/DisableVessel",this);
  fDisableVesselCmd->SetGuidance("Disable a LAV vessel.");
  G4UIparameter* dvStationParameter = new G4UIparameter("Station",'i',false);
  dvStationParameter->SetParameterRange("Station >= 0 && Station <= 11");
  fDisableVesselCmd->SetParameter(dvStationParameter);
  fDisableVesselCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fSelectBlockSimulationCmd = new G4UIcmdWithAString("/Detector/LAV/SelectBlockSimulation",this);
  fSelectBlockSimulationCmd->SetGuidance("Choose simulation model for PbGl block.");
  fSelectBlockSimulationCmd->SetParameterName("BlockSimulation",false);
  fSelectBlockSimulationCmd->SetCandidates("standard optical");

  fEfficiencyMatrixCmd = new G4UIcmdWithAString("/Detector/LAV/EfficiencyMatrix",this);
  fEfficiencyMatrixCmd->SetGuidance("Select efficiency matrix path.");
  fEfficiencyMatrixCmd->SetParameterName("FilePath",false);

  fTimeMatrixCmd = new G4UIcmdWithAString("/Detector/LAV/TimeMatrix",this);
  fTimeMatrixCmd->SetGuidance("Select time matrix path.");
  fTimeMatrixCmd->SetParameterName("FilePath",false);

}

LAVDetectorMessenger::~LAVDetectorMessenger()
{

  delete fLAVDetectorDir;

  delete fEnableAllStationsCmd;
  delete fDisableAllStationsCmd;
  delete fEnableAllLayersCmd;
  delete fDisableAllLayersCmd;
  delete fEnableAllBananasCmd;
  delete fDisableAllBananasCmd;
  delete fEnableAllVesselsCmd;
  delete fDisableAllVesselsCmd;

  delete fEnableStationCmd;
  delete fDisableStationCmd;
  delete fEnableLayerCmd;
  delete fDisableLayerCmd;
  delete fEnableBananaCmd;
  delete fDisableBananaCmd;
  delete fEnableVesselCmd;
  delete fDisableVesselCmd;

  delete fSelectBlockSimulationCmd;
  delete fEfficiencyMatrixCmd;
  delete fTimeMatrixCmd;
}

void LAVDetectorMessenger::SetNewValue(G4UIcommand* cmd, G4String par)
{

  if ( cmd == fEnableStationCmd || cmd == fDisableStationCmd ) {
    G4int s; std::istringstream is(par); is >> s;
    if( cmd == fEnableStationCmd ) fLAVDetector->EnableStation(s);
    if( cmd == fDisableStationCmd ) fLAVDetector->DisableStation(s);
  }

  if ( cmd == fEnableLayerCmd || cmd == fDisableLayerCmd ) {
    G4int s,l; std::istringstream is(par); is >> s >> l;
    if( cmd == fEnableLayerCmd ) fLAVDetector->EnableLayer(s,l);
    if( cmd == fDisableLayerCmd ) fLAVDetector->DisableLayer(s,l);
  }

  if ( cmd == fEnableBananaCmd || cmd == fDisableBananaCmd ) {
    G4int s,l,b; std::istringstream is(par); is >> s >> l >> b;
    if( cmd == fEnableBananaCmd ) fLAVDetector->EnableBanana(s,l,b);
    if( cmd == fDisableBananaCmd ) fLAVDetector->DisableBanana(s,l,b);
  }

  if ( cmd == fEnableVesselCmd || cmd == fDisableVesselCmd ) {
    G4int s; std::istringstream is(par); is >> s;
    if( cmd == fEnableVesselCmd ) fLAVDetector->EnableVessel(s);
    if( cmd == fDisableVesselCmd ) fLAVDetector->DisableVessel(s);
  }

  if ( cmd == fEnableAllStationsCmd )  fLAVDetector->EnableAllStations();
  if ( cmd == fDisableAllStationsCmd ) fLAVDetector->DisableAllStations();

  if ( cmd == fEnableAllLayersCmd )  fLAVDetector->EnableAllLayers();
  if ( cmd == fDisableAllLayersCmd ) fLAVDetector->DisableAllLayers();

  if ( cmd == fEnableAllBananasCmd )  fLAVDetector->EnableAllBananas();
  if ( cmd == fDisableAllBananasCmd ) fLAVDetector->DisableAllBananas();

  if ( cmd == fEnableAllVesselsCmd )  fLAVDetector->EnableAllVessels();
  if ( cmd == fDisableAllVesselsCmd ) fLAVDetector->DisableAllVessels();

  if ( cmd == fSelectBlockSimulationCmd ) fLAVDetector->SetBlockSimulation(par);
  if ( cmd == fEfficiencyMatrixCmd ) LAVGeometryParameters::GetInstance()->SetLAVEfficiencyMatrix(par);  
  if ( cmd == fTimeMatrixCmd ) LAVGeometryParameters::GetInstance()->SetLAVTimeMatrix(par);

}
