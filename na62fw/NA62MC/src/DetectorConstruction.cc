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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#include "DetectorConstruction.hh"
#include "DetectorMessenger.hh"

#include "G4Material.hh"
#include "G4NistManager.hh"
//#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
//#include "globals.hh"
#include "G4VisAttributes.hh"
//#include "G4Colour.hh"

#include "CedarDetector.hh"
#include "CHANTIDetector.hh"
#include "CHODDetector.hh"
#include "GigaTrackerDetector.hh"
#include "HACDetector.hh"
#include "IRCDetector.hh"
#include "LAVDetector.hh"
#include "LKrDetector.hh"
#include "MUV0Detector.hh"
#include "MUV1Detector.hh"
#include "MUV2Detector.hh"
#include "MUV3Detector.hh"
#include "NewCHODDetector.hh"
#include "RICHDetector.hh"
#include "SACDetector.hh"
#include "SpectrometerDetector.hh"

#include "GeometryParameters.hh"
#include "RootIOManager.hh"
#include "MagneticField.hh"

#ifdef G4LIB_USE_GDML
#include "G4GDMLParser.hh"
#endif
#include "G4RunManager.hh"
#include "G4SDManager.hh"
#include "G4GeometryManager.hh"
#include "G4PhysicalVolumeStore.hh"
#include "G4LogicalVolumeStore.hh"
#include "G4SolidStore.hh"

DetectorConstruction::DetectorConstruction() :
  solidWorld(nullptr),  logicWorld(nullptr),  physiWorld(nullptr),
  fWorldZLength(0), fWorldXLength(0), fWorldYLength(0),
  fGeoPars(nullptr) {
  fMessenger = new DetectorMessenger(this);

  // Add subdetectors names list
  fSubDetectorList.push_back(new NA62VNamed("Cedar"));
  fSubDetectorList.push_back(new NA62VNamed("CHANTI"));
  fSubDetectorList.push_back(new NA62VNamed("CHOD"));
  fSubDetectorList.push_back(new NA62VNamed("GigaTracker"));
  fSubDetectorList.push_back(new NA62VNamed("IRC"));
  fSubDetectorList.push_back(new NA62VNamed("LAV"));
  fSubDetectorList.push_back(new NA62VNamed("LKr"));
  fSubDetectorList.push_back(new NA62VNamed("RICH"));
  fSubDetectorList.push_back(new NA62VNamed("Spectrometer"));
  fSubDetectorList.push_back(new NA62VNamed("SAC"));
  fSubDetectorList.push_back(new NA62VNamed("MUV0"));
  fSubDetectorList.push_back(new NA62VNamed("MUV1"));
  fSubDetectorList.push_back(new NA62VNamed("MUV2"));
  fSubDetectorList.push_back(new NA62VNamed("MUV3"));
  fSubDetectorList.push_back(new NA62VNamed("NewCHOD"));
  fSubDetectorList.push_back(new NA62VNamed("HAC"));

  fCedar = new CedarDetector(0,0);
  fCHANTI = new CHANTIDetector(0,0);
  fCHOD = new CHODDetector(0,0);
  fGigaTracker = new GigaTrackerDetector(0,0);
  fHAC = new HACDetector(0,0);
  fIRC = new IRCDetector(0,0);
  fLAV = new LAVDetector(0,0);
  fLKr = new LKrDetector(0,0);
  fMUV0 = new MUV0Detector(0,0);
  fMUV1 = new MUV1Detector(0,0);
  fMUV2 = new MUV2Detector(0,0);
  fMUV3 = new MUV3Detector(0,0);
  fNewCHOD = new NewCHODDetector(0,0);
  fRICH = new RICHDetector(0,0);
  fSAC = new SACDetector(0,0);
  fSpectrometer = new SpectrometerDetector(0,0);
}

DetectorConstruction::~DetectorConstruction() {
  delete fMessenger;
  delete fCedar;
  delete fCHANTI;
  delete fCHOD;
  delete fGigaTracker;
  delete fHAC;
  delete fIRC;
  delete fLAV;
  delete fLKr;
  delete fMUV0;
  delete fMUV1;
  delete fMUV2;
  delete fMUV3;
  delete fNewCHOD;
  delete fRICH;
  delete fSAC;
  delete fSpectrometer;
}

G4VPhysicalVolume* DetectorConstruction::Construct() {
  DefineMaterials();
  ReadGeometryParameters();

  // Clean old geometry, if any
  G4GeometryManager::GetInstance()->OpenGeometry();
  G4PhysicalVolumeStore::GetInstance()->Clean();
  G4LogicalVolumeStore::GetInstance()->Clean();
  G4SolidStore::GetInstance()->Clean();
  G4LogicalBorderSurface::CleanSurfaceTable();
  G4LogicalSkinSurface::CleanSurfaceTable();

  //--------- Definition of World and creation of Subdetectors ---------

  //------------------------------ 
  // World
  //------------------------------ 
  G4double HalfWorldZLength = 0.5*fWorldZLength;
  G4double HalfWorldXLength = 0.5*fWorldXLength;
  G4double HalfWorldYLength = 0.5*fWorldYLength;

  solidWorld= new G4Box("World",HalfWorldXLength,HalfWorldYLength,HalfWorldZLength);

  logicWorld= new G4LogicalVolume(solidWorld,       // solid
            G4Material::GetMaterial("G4_Galactic"), // material
            "World",                           // name
            0,                                 // field manager 
            0,                                 // sensitive detector
            0);                                // user limits


  physiWorld = new G4PVPlacement(0, // no rotation
            G4ThreeVector(), // at (0,0,0)
            logicWorld,      // its logical volume
            "World",         // its name
            0,               // its mother  volume
            false,           // no boolean operations
            0);              // copy number

  if(FindSubDetector("Cedar")->GetEnabled()){
    fCedar->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fCedar->SetMotherVolume(logicWorld);
    fCedar->CreateGeometry();
  }
  if(FindSubDetector("CHANTI")->GetEnabled()){
    fCHANTI->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fCHANTI->SetMotherVolume(logicWorld);
    fCHANTI->CreateGeometry();
  }
  if(FindSubDetector("CHOD")->GetEnabled()){
    fCHOD->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fCHOD->SetMotherVolume(logicWorld);
    fCHOD->CreateGeometry();
  }
  if(FindSubDetector("GigaTracker")->GetEnabled()){
    fGigaTracker->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fGigaTracker->SetMotherVolume(logicWorld);
    fGigaTracker->CreateGeometry();
  }
  if(FindSubDetector("IRC")->GetEnabled()){
    fIRC->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fIRC->SetMotherVolume(logicWorld);
    fIRC->CreateGeometry();
  }
  if(FindSubDetector("LAV")->GetEnabled()){
    fLAV->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fLAV->SetMotherVolume(logicWorld);
    fLAV->CreateGeometry();
  }
  if(FindSubDetector("LKr")->GetEnabled()){
    fLKr->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fLKr->SetMotherVolume(logicWorld);
    fLKr->CreateGeometry();
  }
  if(FindSubDetector("RICH")->GetEnabled()){
    fRICH->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fRICH->SetMotherVolume(logicWorld);
    fRICH->CreateGeometry();
  }
  if(FindSubDetector("Spectrometer")->GetEnabled()){
    fSpectrometer->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fSpectrometer->SetMotherVolume(logicWorld);
    fSpectrometer->CreateGeometry();
  }
  if(FindSubDetector("SAC")->GetEnabled()){
    fSAC->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fSAC->SetMotherVolume(logicWorld);
    fSAC->CreateGeometry();
  }
  if(FindSubDetector("MUV0")->GetEnabled()){
    fMUV0->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fMUV0->SetMotherVolume(logicWorld);
    fMUV0->CreateGeometry();
  }
  if(FindSubDetector("MUV1")->GetEnabled()){
    fMUV1->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fMUV1->SetMotherVolume(logicWorld);
    fMUV1->CreateGeometry();
  }
  if(FindSubDetector("MUV2")->GetEnabled()){
    fMUV2->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fMUV2->SetMotherVolume(logicWorld);
    fMUV2->CreateGeometry();
  }
  if(FindSubDetector("MUV3")->GetEnabled()){
    fMUV3->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fMUV3->SetMotherVolume(logicWorld);
    fMUV3->CreateGeometry();
  }
  if(FindSubDetector("NewCHOD")->GetEnabled()){
    fNewCHOD->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fNewCHOD->SetMotherVolume(logicWorld);
    fNewCHOD->CreateGeometry();
  }
  if(FindSubDetector("HAC")->GetEnabled()){
    fHAC->SetMaterial(G4Material::GetMaterial("G4_Galactic"));
    fHAC->SetMotherVolume(logicWorld);
    fHAC->CreateGeometry();
  }

  // World is invisible
  logicWorld->SetVisAttributes(G4VisAttributes::Invisible);
  return physiWorld;
}

void DetectorConstruction::DefineMaterials() {
  static G4bool already_called = kFALSE;
  if (already_called) return;
  already_called = kTRUE;

  // Standard materials
  G4NistManager* nistMgr = G4NistManager::Instance();
  nistMgr->FindOrBuildMaterial("G4_Galactic");
  nistMgr->FindOrBuildMaterial("G4_AIR");

  // Use these if you want extensive output about material definition
  nistMgr->SetVerbose(0);
  //nistMgr->PrintElement("all");
  //nistMgr->ListMaterials("all");

  // "Black hole" material: all particles entrering it are suppressed
  new G4Material("NA62BlackHole", 1.0, 1.0*g/mole, 1.0*g/cm3);
}

void DetectorConstruction::ReadGeometryParameters() {
  fGeoPars = GeometryParameters::GetInstance();
  if (fGeoPars->Check())
    G4cout << "!!!!!!!!!!!!!!!! Geometry Conflict !!!!!!!!!!!!!!!!" << G4endl;
  fWorldXLength = fGeoPars->GetWorldXLength();
  fWorldYLength = fGeoPars->GetWorldYLength();
  fWorldZLength = fGeoPars->GetWorldZLength();
}

void DetectorConstruction::UpdateGeometry() {
    SubDetectorList::iterator iSubDetector(fSubDetectorList.begin());
    SubDetectorList::iterator endSubDetector(fSubDetectorList.end());

    G4cout << "=============== SubDetectors Status ================" << G4endl; 
    while (iSubDetector!=endSubDetector){
        if((*iSubDetector)->GetEnabled())
            G4cout << (*iSubDetector)->GetName() << " enabled" << G4endl;
        else
            G4cout << (*iSubDetector)->GetName() << " disabled" << G4endl;
        ++iSubDetector;
    }
    G4cout << "====================================================" << G4endl; 

    G4RunManager::GetRunManager()->DefineWorldVolume(Construct());
}

void DetectorConstruction::GenerateGDML() {
#ifdef G4LIB_USE_GDML
  G4GDMLParser parser;
  parser.Write("NA62.gdml",physiWorld);
#else
  G4cerr << "******************************" << G4endl << "GDML not available!!!!!" << G4endl
	 << "******************************" << G4endl;
#endif
}

NA62VNamed* DetectorConstruction::FindSubDetector(G4String name){
  SubDetectorList::iterator iSubDetector(fSubDetectorList.begin());
  SubDetectorList::iterator endSubDetector(fSubDetectorList.end());

  while (iSubDetector!=endSubDetector){
    if((*iSubDetector)->GetName() == name)
      return (*iSubDetector);
    ++iSubDetector;
  }
  G4cout << "********************** SubDetector " << name << " not found!" << G4endl;
  return 0;
}

void DetectorConstruction::EnableSubDetector(G4String name){
    NA62VNamed* SubDet = FindSubDetector(name);
    if (SubDet) SubDet->SetEnabled(true);
    NA62VRootIO* SubDetIO = RootIOManager::GetInstance()->FindRootIO(name);
    if (SubDetIO) SubDetIO->SetEnabled(true);
}

void DetectorConstruction::DisableSubDetector(G4String name){
    NA62VNamed* SubDet = FindSubDetector(name);
    if (SubDet) SubDet->SetEnabled(false);
    NA62VRootIO* SubDetIO = RootIOManager::GetInstance()->FindRootIO(name);
    if (SubDetIO) SubDetIO->SetEnabled(false);
}

void DetectorConstruction::SetBlueTubeFieldScale(G4double val) {
  MagneticField::GetInstance()->SetBlueTubeFieldScale(val);
}

void DetectorConstruction::SetMNP33FieldMode(G4bool val) {
  MagneticField::GetInstance()->SetMNP33FieldMode(val);
}

void DetectorConstruction::SetMNP33FieldScale(G4double val) {
  MagneticField::GetInstance()->SetMNP33FieldScale(val);
}
