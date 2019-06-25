//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-03-02
//
// --------------------------------------------------------------

#include "DetectorMessenger.hh"

#include "DetectorConstruction.hh"
#include "G4UIdirectory.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithADouble.hh"
#include "G4UIcmdWithABool.hh"
#include "G4UIcmdWithoutParameter.hh"

DetectorMessenger::DetectorMessenger(DetectorConstruction* Det) : fDetector(Det) {

  fDetectorDir = new G4UIdirectory("/Detector/");
  fDetectorDir->SetGuidance("UI commands of detector geometry");

  fEnableSubDetectorCmd = new G4UIcmdWithAString("/Detector/EnableSubDetector",this);
  fEnableSubDetectorCmd->SetGuidance("Enable subdetector geometry and hits persistency.");
  fEnableSubDetectorCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fDisableSubDetectorCmd = new G4UIcmdWithAString("/Detector/DisableSubDetector",this);
  fDisableSubDetectorCmd->SetGuidance("Disable subdetector geometry and hits persistency.");
  fDisableSubDetectorCmd->AvailableForStates(G4State_PreInit,G4State_Idle);

  fGenerateGDMLCmd = new G4UIcmdWithoutParameter("/Detector/GenerateGDML",this);
  fGenerateGDMLCmd->SetGuidance("Generate GDML file.");
  fGenerateGDMLCmd->AvailableForStates(G4State_Idle);

  fPathToLKrShowersDb = new G4UIcmdWithAString("/Detector/PathToLKrShowersDb",this);
  fPathToLKrShowersDb->SetDefaultValue("null");
  fPathToLKrShowersDb->SetGuidance("Path to file containing set of showers to perform fast simulation");
  fPathToLKrShowersDb->AvailableForStates(G4State_PreInit,G4State_Idle);

  fUpdateGeometryCmd = new G4UIcmdWithoutParameter("/Detector/UpdateGeometry",this);
  fUpdateGeometryCmd->SetGuidance("Generate GDML file.");
  fUpdateGeometryCmd->AvailableForStates(G4State_Idle);

  // Magnetic fields
  fBlueTubeFieldScaleCmd = new G4UIcmdWithADouble("/Detector/MagneticField/BlueTube/Scale",this);
  fBlueTubeFieldScaleCmd->AvailableForStates(G4State_Idle);

  fMNP33FieldModeCmd = new G4UIcmdWithABool("/Detector/MagneticField/MNP33/Detailed",this);
  fMNP33FieldModeCmd->AvailableForStates(G4State_Idle);

  fMNP33FieldScaleCmd = new G4UIcmdWithADouble("/Detector/MagneticField/MNP33/Scale",this);
  fMNP33FieldScaleCmd->AvailableForStates(G4State_Idle);
}

DetectorMessenger::~DetectorMessenger() {
  delete fDetectorDir;  
  delete fEnableSubDetectorCmd;
  delete fDisableSubDetectorCmd;
  delete fGenerateGDMLCmd;
  delete fUpdateGeometryCmd;
  delete fPathToLKrShowersDb;
  delete fBlueTubeFieldScaleCmd;
  delete fMNP33FieldModeCmd;
  delete fMNP33FieldScaleCmd;
}

void DetectorMessenger::SetNewValue(G4UIcommand* cmd, G4String val) {
  G4cout << cmd->GetCommandPath() << " " << val << G4endl;

  if (cmd==fEnableSubDetectorCmd)  fDetector->EnableSubDetector(val);
  if (cmd==fDisableSubDetectorCmd) fDetector->DisableSubDetector(val);
  if (cmd==fPathToLKrShowersDb)    fDetector->SetPathToLKrShowersDb(val);
  if (cmd==fGenerateGDMLCmd)       fDetector->GenerateGDML();
  if (cmd==fUpdateGeometryCmd)     fDetector->UpdateGeometry();

  if (cmd==fBlueTubeFieldScaleCmd) fDetector->SetBlueTubeFieldScale(fBlueTubeFieldScaleCmd->GetNewDoubleValue(val));
  if (cmd==fMNP33FieldModeCmd)     fDetector->SetMNP33FieldMode(fMNP33FieldModeCmd->GetNewBoolValue(val));
  if (cmd==fMNP33FieldScaleCmd)    fDetector->SetMNP33FieldScale(fMNP33FieldScaleCmd->GetNewDoubleValue(val));
}
