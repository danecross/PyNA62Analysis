// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-13
// ---------------------------------------------------------------------

/// \class CedarSphericalMirror
/// \Brief
/// A spherical mirror reflecting Cherenkov light towards a PMT array
/// \EndBrief

#include "G4Sphere.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "CedarSphericalMirror.hh"

#ifdef CEDARRAYTRACING
#include "G4SDManager.hh"
#endif

CedarSphericalMirror::CedarSphericalMirror
(G4Material* Material, G4LogicalVolume* MotherVolume, G4int CopyNumber) :
  NA62VComponent(Material,MotherVolume),
  iSector(CopyNumber+1), // iSector runs from 1 to 8
  fName  (Form("CedarSphericalMirror%d",iSector))
{
  ReadGeometryParameters();
  CreateGeometry();
  DefineOpticalSurface();
  SetProperties();
}

void CedarSphericalMirror::ReadGeometryParameters() {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  fPosition      = GeoPars->GetSphericalMirrorPosition(iSector);
  fSurfaceRadius = GeoPars->GetSphericalMirrorSurfaceRadius();
  fDiameter      = GeoPars->GetSphericalMirrorDiameter();
  fCentralAngle  = GeoPars->GetSphericalMirrorCentralAngle();
  fOpeningAngle  = asin(0.5*fDiameter/fSurfaceRadius);
}

void CedarSphericalMirror::CreateGeometry() {

  G4RotationMatrix *rot = new G4RotationMatrix;
  rot->rotateX(fCentralAngle);
  G4double fThickness = 2*mm;

  fSolidVolume = new G4Sphere
    (fName, fSurfaceRadius-fThickness, fSurfaceRadius,
     0, 360*deg, 0, fOpeningAngle);

  fLogicalVolume = new G4LogicalVolume(fSolidVolume, fMaterial, fName);

#ifdef CEDARRAYTRACING
  G4SDManager* fSDmanager = G4SDManager::GetSDMpointer();
  G4String fCedarSensitiveDetectorName = "/Cedar";
  G4VSensitiveDetector* fCedarSD =
    fSDmanager->FindSensitiveDetector(fCedarSensitiveDetectorName);
  fLogicalVolume->SetSensitiveDetector(fCedarSD);
#endif

  fPhysicalVolume = new G4PVPlacement
    (rot, fPosition, fLogicalVolume, fName, fMotherVolume, false, 0);
}

void CedarSphericalMirror::DefineOpticalSurface() {
  if (!CedarMaterialParameters::GetInstance()->OpticalPropertiesEnabled()) return;

  fOpticalSurface = new G4OpticalSurface(fName);
  fOpticalSurface->SetType(dielectric_metal);
  fOpticalSurface->SetModel(glisur);
  fOpticalSurface->SetFinish(polished);
  fOpticalSurface->SetMaterialPropertiesTable
    (CedarMaterialParameters::GetInstance()->GetSphericalMirrorOpticalSurfacePT());

  fLogicalSkinSurface = new G4LogicalSkinSurface
    (fName, fLogicalVolume, fOpticalSurface);
}

void CedarSphericalMirror::SetProperties() {}
