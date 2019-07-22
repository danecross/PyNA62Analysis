// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (Evgueni.Goudzovski@cern.ch) 2009-11-16
// ---------------------------------------------------------------------

/// \class CedarQFSMagnet
/// \Brief
/// A quadrupole magnet in the beamline between CEDAR and GTK (QUAD 9, QUAD 10)
/// \EndBrief
/// \Detailed
/// The field gradient (dBx/dy and dBy/dx) is the nominal gradient multiplied by a
/// run-dependent scale factor, as required to tune MC beam divergence to the data.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "CedarQFSMagnet.hh"
#include "CedarMaterialParameters.hh"

CedarQFSMagnet::CedarQFSMagnet
(G4Material * Material, G4LogicalVolume * MotherVolume,
 G4ThreeVector Position, G4double Gradient, G4int iCopy) :
  NA62VComponent(Material,MotherVolume),
  fPosition(Position),
  fiCopy   (iCopy),
  fName    (Form("CedarQuadMagnetField%d",     fiCopy)),
  fNameBH  (Form("CedarQuadMagnetBlackHole%d", fiCopy)),
  fNameYoke(Form("CedarQuadMagnetYoke%d",      fiCopy)),
  fGradient(Gradient)
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

void CedarQFSMagnet::ReadGeometryParameters() {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  fInnerRadius = GeoPars->GetExitPipeInnerRadius2();
  fOuterRadius = GeoPars->GetExitPipeOuterRadius2();
  fZLength     = GeoPars->GetQFSMagnetZLength();
}

void CedarQFSMagnet::CreateGeometry() {
  G4double HalfZLength   =   0.5*fZLength;
  G4double startPhiAngle =   0.0*deg;
  G4double deltaPhiAngle = 360.0*deg;

  // The inpout radii are those of the beam pipe.
  // Define the radii of the concentric field + black hole + pipe + yoke structure.
  G4double FieldOuterRadius     = fInnerRadius - 1.0*mm;
  G4double BlackHoleInnerRadius = FieldOuterRadius;
  G4double BlackHoleOuterRadius = fInnerRadius;
  G4double YokeInnerApothem     = fOuterRadius;
  G4double YokeOuterApothem     = 422*mm;

  // Place the iron yoke (a very approximate simulation)
  const G4int nPlanes = 2, nSides = 8;
  G4double z[nPlanes] = {-HalfZLength, HalfZLength};
  G4double rIn[nPlanes], rOut[nPlanes];

  for (G4int i=0; i<nPlanes; i++) {
    rIn[i]  = YokeInnerApothem;
    rOut[i] = YokeOuterApothem;
  }

  G4Polyhedra *fSolidYoke = new G4Polyhedra
    (fNameYoke, 0*deg, 360*deg, nSides, nPlanes, z, rIn, rOut);
  G4LogicalVolume *fLogicYoke = new G4LogicalVolume
    (fSolidYoke, G4Material::GetMaterial("Cedar_StainlessSteel"), fNameYoke, 0, 0, 0);
  G4RotationMatrix *rot = new G4RotationMatrix;
  rot->rotateZ(22.5*degree);
  new G4PVPlacement(rot, fPosition, fLogicYoke, fNameYoke, fMotherVolume, false, 0);

  // Place the volume containing the magnetic field

  fSolidVolume = new G4Tubs
    (fName, 0, FieldOuterRadius, HalfZLength, startPhiAngle, deltaPhiAngle);
  fLogicalVolume = new G4LogicalVolume
    (fSolidVolume, fMaterial, fName, 0, 0, 0);
  fPhysicalVolume = new G4PVPlacement
    (0, fPosition, fLogicalVolume, fName, fMotherVolume, false, 0);

  // Place a protective "black hole" layer

  fSolidBlackHole = new
    G4Tubs(fNameBH, BlackHoleInnerRadius, BlackHoleOuterRadius, HalfZLength,
	   startPhiAngle, deltaPhiAngle);
  fLogicBlackHole = new G4LogicalVolume
    (fSolidBlackHole, G4Material::GetMaterial("NA62BlackHole"), fNameBH, 0, 0, 0);
  new G4PVPlacement(0, fPosition, fLogicBlackHole, fNameBH, fMotherVolume, false, 0);

  // Quadrupole magnetic field

  fMagField = new G4QuadrupoleMagField(fGradient);
  fFieldMgr = new G4FieldManager(fMagField);
  fFieldMgr->SetDetectorField(fMagField);
  fFieldMgr->CreateChordFinder(fMagField);

  fLogicalVolume->SetFieldManager (fFieldMgr, true); // main magnetic field
  fLogicBlackHole->SetFieldManager(fFieldMgr, true); // black hole
}
