// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2012-04-05
// ---------------------------------------------------------------------

/// \class CedarAtexCylinder
/// \Brief
/// 1/8 of the CEDAR ATEX cylinder
/// \EndBrief

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "CedarAtexCylinder.hh"

#include "G4Tubs.hh"
#include "G4SubtractionSolid.hh"

CedarAtexCylinder::CedarAtexCylinder
(G4Material* Material, G4LogicalVolume* MotherVolume, G4int iSector) :
  NA62VComponent(Material,MotherVolume),
  iCopyNumber(iSector),
  fName(Form("CedarAtexCylinder%d", iSector+1))
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

void CedarAtexCylinder::ReadGeometryParameters() {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  fPosition     = GeoPars->GetAtexCylinderPosition();
  fMinRadius    = GeoPars->GetAtexCylinderMinRadius();
  fMaxRadius    = GeoPars->GetAtexCylinderMaxRadius();
  fZLength      = GeoPars->GetAtexCylinderZLength();
  fHoleDiameter = GeoPars->GetAtexCylinderHoleDiameter();
  fHolePosition = GeoPars->GetAtexCylinderHolePosition();
}

void CedarAtexCylinder::CreateGeometry() {

  G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();

  G4Tubs *Tube = new G4Tubs
    ("Tube", fMinRadius, fMaxRadius, 0.5*fZLength,
     90*deg - 180*deg/fNSectors, 360*deg/fNSectors);

  G4Tubs *Hole = new G4Tubs
    ("Hole", 0, 0.5*fHoleDiameter, 5*(fMaxRadius-fMinRadius), 0*deg, 360*deg);

  G4RotationMatrix *rot = new G4RotationMatrix;
  rot->rotateX(90*deg);

  G4SubtractionSolid *Solid = new G4SubtractionSolid
    (fName, Tube, Hole, rot, fHolePosition);

  fLogicalVolume = new G4LogicalVolume(Solid, fMaterial, fName);

  fPhysicalVolume = new G4PVPlacement
    (0, fPosition, fLogicalVolume, fName, fMotherVolume, false, 0);
}
