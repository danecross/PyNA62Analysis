// ---------------------------------------------------------------------
// History:
//
// 2012-06-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - updated location; simulation of optical transmittance
//
// 2012-02-22 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more appropriate geometry parameterisation
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-10-30
// ---------------------------------------------------------------------

/// \class CedarExternalLens
/// \Brief
/// CEDAR "optical cap" external lenses
/// \EndBrief

#include "G4Orb.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "CedarExternalLens.hh"

#include "G4Sphere.hh"
#include "G4IntersectionSolid.hh"

CedarExternalLens::CedarExternalLens
(G4Material* Material, G4LogicalVolume* MotherVolume, G4int iSector) :
  NA62VComponent(Material,MotherVolume),
  iCopyNumber(iSector),
  fName(Form("CedarExternalLens%d", iSector+1))
{
  ReadGeometryParameters();
  CreateGeometry();
  DefineOpticalSurface();
  SetProperties();
}

void CedarExternalLens::ReadGeometryParameters() {

  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();

  iLGType        = GeoPars->GetLightGuideType();
  fPosition      = GeoPars->GetExternalLensPosition();
  fDiameter      = GeoPars->GetExternalLensDiameter();
  fMinThickness  = GeoPars->GetExternalLensMinThickness();
  fZLength       = GeoPars->GetExternalLensMaxThickness(); // this computes max thickness from the other parameters
  fSurfaceRadius = GeoPars->GetExternalLensSurfaceRadius();

  // The position is defined for the "cylindrical" part of the lens.
  // Move it to the centre of the whole lens.

  G4ThreeVector PosCorr = G4ThreeVector(0, 0, 0.5*(fZLength-fMinThickness));
  fPosition += PosCorr;
}

void CedarExternalLens::CreateGeometry() {

  G4Tubs *fSolidDisk = new
    G4Tubs("CedarExternalLensDisk", 0, 0.5*fDiameter, 0.5*fZLength, 0*deg, 360*deg);

  G4Orb *fSphere = new G4Orb("CedarExternalLensSphere", fSurfaceRadius);
  
  G4IntersectionSolid *fSolid = new
    G4IntersectionSolid(fName, fSolidDisk, fSphere, 0,
			G4ThreeVector(0, 0, 0.5*fZLength-fSurfaceRadius));

  fLogicalVolume = new G4LogicalVolume(fSolid, fMaterial, fName);

  fPhysicalVolume = new
    G4PVPlacement(0, fPosition,
		  fLogicalVolume, fName, fMotherVolume,
		  false, 0);
}

void CedarExternalLens::DefineOpticalSurface() {
  if (!CedarMaterialParameters::GetInstance()->OpticalPropertiesEnabled()) return;

  // The 2012 lens (LGType 32) is made of standard uncoated quartz.
  // It is simulated by the standard G4 Fresnel reflections, nothing to be done.
  if (iLGType==32) return;

  // The 2014 lens (LGTypes 48,64) is made of special "external lens" quartz.
  // Its transmittance (absorption length) has been provided by Edmund Optics.
  // No reflections: all losses are included into this parameterisation.

  fOpticalSurface = new G4OpticalSurface(fName);
  fOpticalSurface->SetType(dielectric_dielectric);
  fOpticalSurface->SetModel(glisur);
  fOpticalSurface->SetFinish(polished);
  fOpticalSurface->SetMaterialPropertiesTable
    (CedarMaterialParameters::GetInstance()->GetLensOpticalSurfacePT());

  fLogicalSkinSurface = new
    G4LogicalSkinSurface(fName, fLogicalVolume, fOpticalSurface);
}

void CedarExternalLens::SetProperties() {}
