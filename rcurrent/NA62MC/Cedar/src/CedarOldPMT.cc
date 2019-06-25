// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2012-02-16
// ---------------------------------------------------------------------

/// \class CedarOldPMT
/// \Brief
/// CEDAR old (pre-2012) photomultiplier
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

#include "CedarOldPMT.hh"
#include "CedarLightGuide.hh"
#include "CedarMaterialParameters.hh"

CedarOldPMT::CedarOldPMT(G4Material* Material, G4LogicalVolume* MotherVolume, G4int Sector) :
  NA62VComponent(Material, MotherVolume),
  fName       (Form("CedarOldPMT%d", fSector)),
  fWindowName (Form("CedarOldPMTWindow%d", fSector)),
  fCathodeName(Form("CedarOldPMTCathode%d", fSector)),
  fSector     (Sector),
  fCopyNumber (100*fSector)
{
  ReadGeometryParameters();
  CreateGeometry();
  DefineOpticalSurface();
  SetProperties();
}

void CedarOldPMT::ReadGeometryParameters() {
  CedarGeometryParameters* geoPars = CedarGeometryParameters::GetInstance();
  fPosition             = geoPars->GetOldPMTPosition();
  fLength               = geoPars->GetOldPMTLength(); 
  fRadius               = geoPars->GetOldPMTRadius();
  fWindowLength         = geoPars->GetOldPMTWindowLength(); 
  fWindowRadius         = geoPars->GetOldPMTWindowRadius();
  fPhotoCathodeLength   = geoPars->GetOldPMTPhotoCathodeLength(); 
  fPhotoCathodeRadius   = geoPars->GetOldPMTPhotoCathodeRadius();
  fWindowMaterial       = G4Material::GetMaterial("Cedar_IdealQuartz");
  fPhotoCathodeMaterial = G4Material::GetMaterial("Cedar_Quartz");
}

void CedarOldPMT::CreateGeometry() {

  // The PMT mother volume
  G4Tubs* fTube  = new G4Tubs("CedarOldPMT", 0, fRadius, 0.5*fLength, 0, 360*deg);
  fLogicalVolume = new G4LogicalVolume(fTube, fMaterial, fName);

  fPhysicalVolume = new
    G4PVPlacement(0, fPosition, fLogicalVolume, fName,
		  fMotherVolume, false, fCopyNumber);

  // Glass window
  G4Tubs* fWindowTube = new
    G4Tubs(fWindowName, 0, fWindowRadius, 0.5*fWindowLength, 0, 360*deg);
  fWindowLogicalVolume = new
    G4LogicalVolume(fWindowTube, fWindowMaterial, fWindowName);

  G4ThreeVector	translationWindow =
    G4ThreeVector(0, 0, 0.5*(fLength-fWindowLength));

  fWindowPhysicalVolume = new
    G4PVPlacement (0, translationWindow, fWindowLogicalVolume, fWindowName,
		   fLogicalVolume, false, fCopyNumber);

  // Photocathode
  G4Tubs* fCathodeTube = new
    G4Tubs(fCathodeName, 0, fPhotoCathodeRadius, 0.5*fPhotoCathodeLength, 0, 360*deg);
  fCathodeLogicalVolume = new
    G4LogicalVolume(fCathodeTube, fPhotoCathodeMaterial, fCathodeName);

  G4ThreeVector	translationCathode =
    G4ThreeVector(0, 0, 0.5*fLength-fWindowLength-0.5*fPhotoCathodeLength);

  fCathodePhysicalVolume = new
    G4PVPlacement (0, translationCathode, fCathodeLogicalVolume, fCathodeName,
		   fLogicalVolume, false, fCopyNumber);

  // Associate the photocathode to the Cedar sensitive detector
  G4SDManager* fSDmanager = G4SDManager::GetSDMpointer();
  G4String fCedarSensitiveDetectorName = "/Cedar";
  G4VSensitiveDetector* fCedarSD =
    fSDmanager->FindSensitiveDetector(fCedarSensitiveDetectorName);
  fCathodeLogicalVolume->SetSensitiveDetector(fCedarSD);
}

void CedarOldPMT::DefineOpticalSurface() {

  // Non-reflective window (reflections are parameterized by QE)

  fOpticalSurface = new G4OpticalSurface("CedarOldPMTWindowSurface");
  fOpticalSurface->SetType(dielectric_dielectric);
  fOpticalSurface->SetModel(glisur);
  fOpticalSurface->SetFinish(polished);
  fOpticalSurface->SetMaterialPropertiesTable
    (CedarMaterialParameters::GetInstance()->GetLensOpticalSurfacePT());

  new G4LogicalSkinSurface("CedarOldPMTWindowOpticalSurface",
			   fWindowLogicalVolume, fOpticalSurface);
}

void CedarOldPMT::SetProperties() {}
