// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2012-06-15
// ---------------------------------------------------------------------

/// \class CedarSphericalMirrorMount
/// \Brief
/// 1/8 of the CEDAR spherical mount cylinder
/// \EndBrief

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "CedarSphericalMirrorMount.hh"

#include "G4Tubs.hh"
#include "G4SubtractionSolid.hh"

CedarSphericalMirrorMount::CedarSphericalMirrorMount
(G4Material* Material, G4LogicalVolume* MotherVolume, G4int iSector) :
  NA62VComponent(Material,MotherVolume),
  iCopyNumber(iSector),
  fName      (Form("CedarSphericalMirrorMount%d",iSector+1)),
  fName2     (Form("CedarSupportCylinder%d",iSector+1))
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

CedarSphericalMirrorMount::~CedarSphericalMirrorMount(){}

void CedarSphericalMirrorMount::ReadGeometryParameters() {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  fPosition          = GeoPars->GetSphericalMirrorMountPosition();
  fLength            = GeoPars->GetSphericalMirrorMountZLength();
  fSupportRingLength = GeoPars->GetSphericalMirrorMountSupportRingZLength();
  fSupportRingRin    = GeoPars->GetSphericalMirrorMountSupportRingRin();
  fSupportRingRout   = GeoPars->GetSphericalMirrorMountSupportRingRout();
  fHoleDiameter      = GeoPars->GetSphericalMirrorMountSupportRingHoleDia();
  fHoleRadialOffset  = GeoPars->GetQuartzWindowRadialOffset();
}

void CedarSphericalMirrorMount::CreateGeometry() {

  G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();

  // define and place the "long" cylinder

  const G4int nPlanes = 8;
  G4double z[nPlanes], rInner[nPlanes], rOuter[nPlanes];

  z[0] = -0.5*fLength;
  z[1] = z[2] = z[0] + 105*mm;
  z[3] = z[4] = z[0] + 165*mm;
  z[5] = z[6] = z[0] + 200*mm;
  z[7] = +0.5*fLength;

  for (G4int i=0; i<nPlanes; i++) rInner[i] = 68*mm;
  rOuter[0] = rOuter[1] = 80*mm;
  rOuter[2] = rOuter[3] = 87*mm;
  rOuter[4] = rOuter[5] = 76*mm;
  rOuter[6] = rOuter[7] = 80*mm;

  G4Polycone *fSolid = new
    G4Polycone("CedarVessel",
               90*deg - 180*deg/fNSectors, 360*deg/fNSectors,
               nPlanes, z, rInner, rOuter);

  fLogicalVolume = new G4LogicalVolume(fSolid, fMaterial, fName);

  fPhysicalVolume = new
    G4PVPlacement(0, fPosition,
		  fLogicalVolume, fName, fMotherVolume, false, 0);

  // define and place the support cylinder with a hole

  G4Tubs *fTube = new
    G4Tubs("fTube", fSupportRingRin, fSupportRingRout, 0.5*fSupportRingLength,
           90*deg - 180*deg/fNSectors, 360*deg/fNSectors);

  G4Tubs *fHole = new
    G4Tubs("fHole", 0, 0.5*fHoleDiameter, 0.55*fSupportRingLength, 0*deg, 360*deg);

  G4SubtractionSolid *fSupportSolid = new
    G4SubtractionSolid(fName2, fTube, fHole, 0, G4ThreeVector(0.0, fHoleRadialOffset, 0.0));

  G4LogicalVolume *fSupportLogicalVolume = new
    G4LogicalVolume(fSupportSolid, fMaterial, fName2);

  G4double zpos = fPosition.getZ() + 0.5*fLength + 0.5*fSupportRingLength;

  new G4PVPlacement(0, G4ThreeVector(0, 0, zpos),
		    fSupportLogicalVolume, fName2, fMotherVolume, false, 0);
}

void CedarSphericalMirrorMount::SetProperties() {
}
