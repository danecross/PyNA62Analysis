// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-16
// ---------------------------------------------------------------------

/// \class CedarManginMirror
/// \Brief
/// The CEDAR Mangin mirror
/// \EndBrief

#include "G4Tubs.hh"
#include "G4Orb.hh"
#include "G4Sphere.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "CedarManginMirror.hh"
#include "CedarMaterialParameters.hh"

CedarManginMirror::CedarManginMirror
(G4Material* Material, G4LogicalVolume* MotherVolume) :
  NA62VComponent(Material,MotherVolume) {
  ReadGeometryParameters();
  CreateGeometry();
  DefineOpticalSurface();
  SetProperties();
}

void CedarManginMirror::ReadGeometryParameters() {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  fPosition                = GeoPars->GetManginMirrorPosition();
  fZLength                 = GeoPars->GetManginMirrorZLength();
  fCoatingZLength          = GeoPars->GetManginMirrorCoatingZLength();
  fInnerRadius             = GeoPars->GetManginMirrorInnerRadius();
  fOuterRadius             = GeoPars->GetManginMirrorOuterRadius();
  fReflectingSurfaceRadius = GeoPars->GetManginMirrorReflectingSurfaceRadius();
  fRefractingSurfaceRadius = GeoPars->GetManginMirrorRefractingSurfaceRadius();
}

void CedarManginMirror::CreateGeometry() {

  // basic & auxiliary solids
  G4Tubs *fTube = new
    G4Tubs("CedarManginMirrorTube", fInnerRadius, fOuterRadius, fZLength, 0, 360*deg);

  G4Orb *fSmallSphere = new
    G4Orb("CedarManginMirrorSmallSphere", fRefractingSurfaceRadius);

  G4Orb *fLargeSphere = new
    G4Orb("CedarManginMirrorLargeSphere", fReflectingSurfaceRadius);

  G4Sphere *fCoatingSphere = new
    G4Sphere("CedarManginMirrorCoatingSphere",
	     fReflectingSurfaceRadius, fReflectingSurfaceRadius+fCoatingZLength,
	     0, 360*deg, 0, 180*deg);

  G4IntersectionSolid *fIntermediate = new
    G4IntersectionSolid("CedarManginWindowIntermediateSolid", fTube, fLargeSphere, 0,
			G4ThreeVector(0, 0, -fReflectingSurfaceRadius+0.5*fZLength));

  // lens and coating solids
  G4SubtractionSolid *fLensSolidVolume = new
    G4SubtractionSolid("CedarManginMirrorLens", fIntermediate, fSmallSphere, 0,
		       G4ThreeVector(0, 0, -fRefractingSurfaceRadius-0.5*fZLength));

  G4IntersectionSolid *fCoatingSolidVolume = new
    G4IntersectionSolid("CedarManginMirrorCoating", fTube, fCoatingSphere, 0,
			G4ThreeVector(0, 0, -fReflectingSurfaceRadius+0.5*fZLength));

  // logical volumes
  G4LogicalVolume *fLensLogicalVolume = new
    G4LogicalVolume(fLensSolidVolume, fMaterial, "CedarManginMirrorLens");

  G4LogicalVolume *fCoatingLogicalVolume = new
    G4LogicalVolume(fCoatingSolidVolume,
		    G4Material::GetMaterial("G4_Al"),
		    "CedarManginMirrorCoating");

  // physical volumes
  fCoatingPhysicalVolume = new G4PVPlacement
    (0, fPosition, fCoatingLogicalVolume,
     "CedarManginMirrorCoating", fMotherVolume, false, 0);

  fLensPhysicalVolume = new G4PVPlacement
    (0, fPosition, fLensLogicalVolume,
     "CedarManginMirrorLens", fMotherVolume, false, 0);
}

void CedarManginMirror::DefineOpticalSurface() {

  // Reflecting (back) surface
  fReflectingOpticalSurface = new G4OpticalSurface("CedarManginMirrorReflectingSurface");
  fReflectingOpticalSurface->SetType(dielectric_metal);
  fReflectingOpticalSurface->SetModel(glisur);
  fReflectingOpticalSurface->SetFinish(polished);
  fReflectingOpticalSurface->SetMaterialPropertiesTable
    (CedarMaterialParameters::GetInstance()->GetManginMirrorOpticalSurfacePT());

  // Refracting (front) surface
  fRefractingOpticalSurface = new G4OpticalSurface("CedarManginMirrorRefractingSurface");
  fRefractingOpticalSurface->SetType(dielectric_dielectric);
  fRefractingOpticalSurface->SetModel(glisur);
  fRefractingOpticalSurface->SetFinish(polished);
  fRefractingOpticalSurface->SetMaterialPropertiesTable
    (CedarMaterialParameters::GetInstance()->GetLensOpticalSurfacePT());
}

void CedarManginMirror::SetProperties() {}
