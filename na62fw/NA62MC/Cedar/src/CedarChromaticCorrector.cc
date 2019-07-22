// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-16
// ---------------------------------------------------------------------

/// \class CedarChromaticCorrector
/// \Brief
/// The CEDAR chromatic corrector lens
/// \EndBrief

#include "G4Tubs.hh"
#include "G4Orb.hh"
#include "G4IntersectionSolid.hh"
#include "G4LogicalSkinSurface.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"
#include "G4VisAttributes.hh"
#include "CedarChromaticCorrector.hh"
#include "CedarMaterialParameters.hh"

CedarChromaticCorrector::CedarChromaticCorrector
(G4Material* Material, G4LogicalVolume* MotherVolume) :
  NA62VComponent(Material, MotherVolume) {
  ReadGeometryParameters();
  CreateGeometry();
  DefineOpticalSurface();
  SetProperties();
}

void CedarChromaticCorrector::ReadGeometryParameters() {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  fName              = "CedarChromaticCorrector";
  fPosition          = GeoPars->GetChromaticCorrectorPosition();
  fZLength           = GeoPars->GetChromaticCorrectorZLength();
  fInnerRadius       = GeoPars->GetChromaticCorrectorInnerRadius();
  fOuterRadius       = GeoPars->GetChromaticCorrectorOuterRadius();
  fRearSurfaceRadius = GeoPars->GetChromaticCorrectorRearSurfaceRadius();
}

void CedarChromaticCorrector::CreateGeometry() {

  G4Tubs *fTube = new
    G4Tubs("CedarChromaticCorrectorTube", fInnerRadius, fOuterRadius,
	   0.5*fZLength, 0, 360*deg);

  G4Orb *fSphere = new
    G4Orb("CedarChromaticCorrectorSphere", fRearSurfaceRadius);

  // set lens thickness of fZLength at R=0
  G4double dL = -fRearSurfaceRadius+0.5*fZLength;

  // set lens thickness of fZLength at R=Rinner
  //dL += (fRearSurfaceRadius - sqrt(fRearSurfaceRadius*fRearSurfaceRadius - fInnerRadius*fInnerRadius));

  fSolidVolume = new
    G4IntersectionSolid(fName, fTube, fSphere, 0, G4ThreeVector(0, 0, dL));

  fLogicalVolume = new G4LogicalVolume(fSolidVolume, fMaterial, fName);

  fPhysicalVolume = new
    G4PVPlacement(0, fPosition,	fLogicalVolume, fName, fMotherVolume, false, 0);
}

void CedarChromaticCorrector::DefineOpticalSurface() {
  if (!CedarMaterialParameters::GetInstance()->OpticalPropertiesEnabled()) return;

  fOpticalSurface = new G4OpticalSurface(fName);
  fOpticalSurface->SetType(dielectric_dielectric);
  fOpticalSurface->SetModel(glisur);
  fOpticalSurface->SetFinish(polished);
  fOpticalSurface->SetMaterialPropertiesTable
    (CedarMaterialParameters::GetInstance()->GetLensOpticalSurfacePT());

  fLogicalSkinSurface = new
    G4LogicalSkinSurface(fName, fLogicalVolume, fOpticalSurface);
}

void CedarChromaticCorrector::SetProperties() {}
