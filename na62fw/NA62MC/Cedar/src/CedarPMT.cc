// ---------------------------------------------------------------------
// History:
//
// 2012-06-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more precise geometry description
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - compatibility to the general simulation
//
// Created by Karim Massri (karim.massri@cern.ch) 2011-04-28
// ---------------------------------------------------------------------

/// \class CedarPMT
/// \Brief
/// CEDAR photomultiplier
/// \EndBrief

#include "G4Tubs.hh"
#include "G4Cons.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4SDManager.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "CedarPMT.hh"
#include "CedarLightGuide.hh"
#include "CedarMaterialParameters.hh"

CedarPMT::CedarPMT(G4Material* Material, G4LogicalVolume* MotherVolume,
		   G4int Sector, G4int Row, G4int Cone,
		   G4RotationMatrix* Rot, G4ThreeVector Pos) :
  NA62VComponent(Material, MotherVolume) {

  CedarGeometryParameters* geoPars = CedarGeometryParameters::GetInstance();

  fSector        = Sector;
  fRow           = Row;
  fCone          = Cone;
  fPositionID    = 111 + 100*fSector + 10*fRow + fCone + 2*(fRow==0);
  fSequentialID  = geoPars->GetPMTSequentialID(fPositionID);
  fRotation      = Rot;
  fPosition      = Pos;
  fName          = Form("CedarPMT%03d",          fPositionID);
  fPreWindowName = Form("CedarPMTPreWindow%03d", fPositionID);
  fWindowName    = Form("CedarPMTWindow%03d",    fPositionID);
  fCathodeName   = Form("CedarPMTCathode%03d",   fPositionID);

  ReadGeometryParameters();
  CreateGeometry();
  DefineOpticalSurface();
  SetProperties();
}

CedarPMT::~CedarPMT() {}

void CedarPMT::ReadGeometryParameters() {
  CedarGeometryParameters* geoPars = CedarGeometryParameters::GetInstance();
  fRadius               = 0.5 * geoPars->GetPMTDiameter();
  fWindowRadius         = 0.5 * geoPars->GetPMTWindowDiameter();
  fPhotoCathodeRadius   = 0.5 * geoPars->GetPMTPhotoCathodeDiameter();
  fLength               = geoPars->GetPMTLength(); 
  fPreWindowLength      = geoPars->GetPMTPreWindowLength(); 
  fWindowLength         = geoPars->GetPMTWindowLength(); 
  fPhotoCathodeLength   = geoPars->GetPMTPhotoCathodeLength(); 
  fPreWindowMaterial    = G4Material::GetMaterial("Cedar_Nitrogen");
  fWindowMaterial       = G4Material::GetMaterial("Cedar_IdealQuartz");
  fPhotoCathodeMaterial = G4Material::GetMaterial("Cedar_Quartz");
}

void CedarPMT::CreateGeometry() {

  // The PMT mother volume
  G4Tubs* fTube  = new G4Tubs("CedarPMT", 0, fRadius, 0.5*fLength, 0, 360*deg);
  fLogicalVolume = new G4LogicalVolume(fTube, fMaterial, fName);

  fPhysicalVolume = new G4PVPlacement
    (fRotation, fPosition, fLogicalVolume, fName, fMotherVolume, false, fPositionID);

  // Nitrogen volume in front of the glass window
  G4Tubs* fPreWindowTube = new
    G4Tubs(fPreWindowName, 0, fWindowRadius, 0.5*fPreWindowLength, 0, 360*deg);
  fPreWindowLogicalVolume = new
    G4LogicalVolume(fPreWindowTube, fPreWindowMaterial, fPreWindowName);

  G4ThreeVector	translationPreWindow =
    G4ThreeVector(0, 0, 0.5*(fPreWindowLength-fLength));

  fPreWindowPhysicalVolume = new G4PVPlacement
    (0, translationPreWindow, fPreWindowLogicalVolume, fPreWindowName,
     fLogicalVolume, false, fPositionID);

  // Glass window
  G4Tubs* fWindowTube = new
    G4Tubs(fWindowName, 0, fWindowRadius, 0.5*fWindowLength, 0, 360*deg);
  fWindowLogicalVolume = new
    G4LogicalVolume(fWindowTube, fWindowMaterial, fWindowName);

  G4ThreeVector	translationWindow = G4ThreeVector
    (0, 0, 0.5*(fWindowLength-fLength)+fPreWindowLength);

  fWindowPhysicalVolume = new G4PVPlacement
    (0, translationWindow, fWindowLogicalVolume, fWindowName,
     fLogicalVolume, false, fPositionID);

  // Photocathode
  G4Tubs* fCathodeTube = new
    G4Tubs(fCathodeName, 0, fPhotoCathodeRadius, 0.5*fPhotoCathodeLength, 0, 360*deg);
  fCathodeLogicalVolume = new
    G4LogicalVolume(fCathodeTube, fPhotoCathodeMaterial, fCathodeName);

  G4ThreeVector	translationCathode = G4ThreeVector
    (0, 0, 0.5*(fPhotoCathodeLength-fLength)+fPreWindowLength+fWindowLength);

  fCathodePhysicalVolume = new G4PVPlacement
    (0, translationCathode, fCathodeLogicalVolume, fCathodeName,
     fLogicalVolume, false, fPositionID);

  // Associate the photocathode to the Cedar sensitive detector
  G4SDManager* fSDmanager = G4SDManager::GetSDMpointer();
  G4String fCedarSensitiveDetectorName = "/Cedar";
  G4VSensitiveDetector* fCedarSD =
    fSDmanager->FindSensitiveDetector(fCedarSensitiveDetectorName);
  fCathodeLogicalVolume->SetSensitiveDetector(fCedarSD);
}

void CedarPMT::DefineOpticalSurface() {

  if (!CedarMaterialParameters::GetInstance()->OpticalPropertiesEnabled()) return;

  // Non-reflective window (reflections are parameterized by QE)
  fOpticalSurface = new G4OpticalSurface("CedarPMTWindowSurface");
  fOpticalSurface->SetType(dielectric_dielectric);
  fOpticalSurface->SetModel(glisur);
  fOpticalSurface->SetFinish(polished);
  fOpticalSurface->SetMaterialPropertiesTable
    (CedarMaterialParameters::GetInstance()->GetLensOpticalSurfacePT());

  new G4LogicalSkinSurface("CedarPMTWindowOpticalSurface",
			   fWindowLogicalVolume, fOpticalSurface);
}

void CedarPMT::SetProperties() {}
