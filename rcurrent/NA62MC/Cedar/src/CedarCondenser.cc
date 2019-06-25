// ---------------------------------------------------------------------
// History:
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more realistic geometry
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-16
// ---------------------------------------------------------------------

/// \class CedarCondenser
/// \Brief
/// The CEDAR condenser lens
/// \EndBrief

#include "G4Tubs.hh"
#include "G4Trd.hh"
#include "G4Orb.hh"
#include "G4IntersectionSolid.hh"

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "CedarCondenser.hh"
#include "CedarMaterialParameters.hh"

CedarCondenser::CedarCondenser
(G4Material* Material, G4LogicalVolume* MotherVolume, G4int iSector)
  : NA62VComponent(Material,MotherVolume) {

  const G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();

  ReadGeometryParameters();

  fCopyNumber = iSector;
  fName       = Form("CedarCondenser%d", iSector+1);
  fAngle      = (iSector+2.5) * 360*deg / fNSectors;

  G4ThreeVector transverse_offset =
    G4ThreeVector(fRadialOffset*cos(fAngle), fRadialOffset*sin(fAngle), 0);
  fPosition  += transverse_offset;

  fRotationMatrix = new G4RotationMatrix;
  fRotationMatrix->rotateZ(-fAngle-90*deg);

  CreateGeometry();
  DefineOpticalSurface();
  SetProperties();
}

void CedarCondenser::ReadGeometryParameters() {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  fZStart              = GeoPars->GetZCondenserStart();
  fZLength             = GeoPars->GetCondenserZLength();
  fOuterRadius         = GeoPars->GetCondenserOuterRadius();
  fFrontSurfaceRadius  = GeoPars->GetCondenserFrontSurfaceRadius();
  fRadialOffset        = GeoPars->GetCondenserRadialOffset();
  fDistanceToCentre    = GeoPars->GetCondenserDistanceToCentre();
  fInterCondenserAngle = GeoPars->GetInterCondenserAngle();
  fPosition            = GeoPars->GetCondenserPosition();
}

void CedarCondenser::CreateGeometry() {

  const G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();

  // define the solids

  G4Tubs *fTube = new
    G4Tubs("CedarCondenserTube", 0, fOuterRadius,
	   0.5*fZLength, 0, 180*deg);

  // half of the condenser wegde angle
  G4double angle = 180*deg/fNSectors - 0.25*fInterCondenserAngle;

  G4Trd *fWedge = new
    G4Trd("CedarCondenserWedge",
	  (fRadialOffset-fDistanceToCentre)*tan(angle), 0,
	  0.5*fZLength, 0.5*fZLength,
	  0.5*(fRadialOffset-fDistanceToCentre));

  G4Orb *fSphere = new
    G4Orb("CedarCondenserSphere", fFrontSurfaceRadius);

  // build the lens as the intersection of the 3 solids

  G4RotationMatrix *rot = new G4RotationMatrix;
  rot->rotateX(90*deg);
  G4ThreeVector trans (0, 0.5*fRadialOffset+0.5*fDistanceToCentre, 0);

  G4IntersectionSolid *fTubeAndWedge = new
    G4IntersectionSolid ("CedarCondenserTubeAndWedge",
			 fTube, fWedge, rot, trans);

  fSolidVolume = new
    G4IntersectionSolid(fName,
			fTubeAndWedge, fSphere, 0,
			G4ThreeVector(0, 0, fFrontSurfaceRadius-0.5*fZLength));

  // logical volume and placement

  fLogicalVolume = new G4LogicalVolume(fSolidVolume, fMaterial, fName);

  fPhysicalVolume = new
    G4PVPlacement(fRotationMatrix, fPosition, fLogicalVolume,
		  fName, fMotherVolume,
		  false, fCopyNumber);
}

void CedarCondenser::DefineOpticalSurface() {

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

void CedarCondenser::SetProperties() {}
