// ---------------------------------------------------------------------
// History:
//
// 2014-04-11 Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// ---------------------------------------------------------------------

/// \class CedarExitPipe
/// \Brief
/// Build the Cedar exit beam pipes, from exit window to end of Cedar RR
/// \EndBrief

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "CedarExitPipe.hh"
#include "CedarMaterialParameters.hh"

CedarExitPipe::CedarExitPipe (G4Material *Material, G4LogicalVolume *MotherVolume) : 
  NA62VComponent(Material,MotherVolume) {

  ReadGeometryParameters();
  CedarMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

CedarExitPipe::~CedarExitPipe() {}

void CedarExitPipe::ReadGeometryParameters() {

  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();

  fZLength            = GeoPars->GetExitPipeZLength();
  fInnerRadius1       = GeoPars->GetExitPipeInnerRadius1(); // upstream
  fOuterRadius1       = GeoPars->GetExitPipeOuterRadius1();
  fInnerRadius2       = GeoPars->GetExitPipeInnerRadius2(); // downstream
  fOuterRadius2       = GeoPars->GetExitPipeOuterRadius2();
  fPosition           = GeoPars->GetExitPipePosition();
}

void CedarExitPipe::CreateGeometry() {

  const G4int nPlanes = 4;
  G4double z[nPlanes], rIn[nPlanes], rOut[nPlanes];

  z[0] = -0.5*fZLength;
  z[1] =  z[0] + 800*mm;
  z[2] =  z[1] +  10*mm;
  z[3] = -z[0];

  rIn[0] = fInnerRadius1;
  rIn[1] = rIn[0];
  rIn[2] = fInnerRadius2;
  rIn[3] = rIn[2];

  rOut[0] = fOuterRadius1;
  rOut[1] = rOut[0];
  rOut[2] = fOuterRadius2;
  rOut[3] = rOut[2];

  G4Polycone *fSolidExitPipe = new G4Polycone
    ("CedarExitPipe", 0*deg, 360*deg, nPlanes, z, rIn, rOut);

  fLogicalVolume = new G4LogicalVolume
    (fSolidExitPipe, fMaterial, "CedarExitPipe", 0, 0, 0);

  fPhysicalVolume = new G4PVPlacement
    (0, fPosition, fLogicalVolume, "CedarExitPipe", fMotherVolume, false, 0);
}

void CedarExitPipe::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(0.5,0.5,0.5));
}
