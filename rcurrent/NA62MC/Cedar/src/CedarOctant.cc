// ---------------------------------------------------------------------
// History:
//
// 2012-06-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Atex cylinder added, minor updates
//
// 2012-02-22 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more photodetector configurations added
//
// 2011-11-18 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - external cap lenses added
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-13
// ---------------------------------------------------------------------

/// \class CedarOctant
/// \Brief
/// One of the eight sectors containing the light transport system and PMTs
/// \EndBrief

#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "CedarOctant.hh"

//-----------------------------------------------------------------------------
// Octants are numbered 1-8, ordered clockwise looking in the K beam direction.
// The Saleve (Jura) octants are numbered 1-4 (5-8).
// Octant 1 is top Saleve (=top right looking along the K beam).
// The X axis is directed to the Jura (=left looking along the K beam).
// Octant locations:
// 1,2: x<0, y>0      3,4: x<0, y<0
// 5,6: x>0, y<0      7,8: x>0, y>0
// Reference frame of the octant: the octant centre of symmetry axis is Y.
//-----------------------------------------------------------------------------

CedarOctant::CedarOctant
(G4Material* Material, G4LogicalVolume* MotherVolume, G4int iSector) :
  NA62VComponent(Material,MotherVolume) {

  G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();
  fCopyNumber     = iSector; // runs from zero
  fName           = Form("CedarOctant%d", iSector+1);
  fCentralAngle   = (360*deg / fNSectors) * (iSector+0.5);
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

void CedarOctant::ReadGeometryParameters() {

  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();

  iLGType    = GeoPars->GetLightGuideType();
  iEnabled   = (GeoPars->GetOctantsEnabled() >> fCopyNumber) & 0x1;
  fPosition  = GeoPars->GetFrontPipePosition();
  fMinRadius = GeoPars->GetFrontPipeOuterRadius();
  fMaxRadius = 0.5*GeoPars->GetRotBoxXLength();
  fZLength   = GeoPars->GetFrontPipeZLength();
}

void CedarOctant::CreateGeometry() {

  G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();

  // Place the octant volume
  G4RotationMatrix *rot = new G4RotationMatrix();
  rot->rotateZ(-fCentralAngle);

  // The octant is centered along the Y axis
  fSolidVolume = new G4Tubs
    (fName, fMinRadius, fMaxRadius,
     0.5*fZLength, 90*deg - 180*deg/fNSectors, 360*deg/fNSectors);

  fLogicalVolume = new G4LogicalVolume(fSolidVolume, fMaterial, fName);

  fPhysicalVolume = new G4PVPlacement
    (rot, fPosition, fLogicalVolume, fName, fMotherVolume, false, fCopyNumber);

  /////////////////////////////////////////////

  if (iLGType!=1 && iLGType!=3) { // KTAG 2012+ configurations

    // Place 1/8 of the ATEX cylinder
    fAtexCylinder = new CedarAtexCylinder
      (G4Material::GetMaterial("G4_Al"), fLogicalVolume, fCopyNumber);

    // Place 1/8 of the mirror mount
    fMirrorMount = new CedarSphericalMirrorMount
      (G4Material::GetMaterial("G4_Al"), fLogicalVolume, fCopyNumber);

    if (iEnabled) {
 
      // Place an external "optical cap" lens
      fExternalLens = new CedarExternalLens
	(G4Material::GetMaterial("Cedar_ExternalLensQuartz"),
	 fLogicalVolume, fCopyNumber);

      // Place a spherical mirror
      fMirror = new CedarSphericalMirror
	(G4Material::GetMaterial("G4_Al"), fLogicalVolume, fCopyNumber);

      // Place a lightguide
      fLightGuide = new	CedarLightGuide
	(G4Material::GetMaterial("G4_Al"), fLogicalVolume, fCopyNumber);
    }
  }

  /////////////////////////////////////////////

  if (iLGType==1 && iEnabled) { // pre-2011 configuration
    fOldPMT = new CedarOldPMT(G4Material::GetMaterial("G4_Al"), fLogicalVolume, fCopyNumber);
  }

  if (iLGType==3 && iEnabled) { // test beam 2011
    fLightGuide2011 = new
      CedarLightGuide2011(G4Material::GetMaterial("G4_Al"), fLogicalVolume, fCopyNumber);
  }
}

void CedarOctant::SetProperties() {}
