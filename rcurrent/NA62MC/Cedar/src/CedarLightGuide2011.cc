// ---------------------------------------------------------------------
// History:
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - cones and PMTs added
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-13
// ---------------------------------------------------------------------

/// \class CedarLightGuide2011
/// \Brief
/// CEDAR 3-PMT holder cylinder used during the 2011 test beam
/// \EndBrief

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "CedarLightGuide2011.hh"

CedarLightGuide2011::CedarLightGuide2011
(G4Material* Material, G4LogicalVolume* MotherVolume, G4int iSector) :
  NA62VComponent(Material,MotherVolume),
  iCopyNumber(iSector),
  fName      (Form("CedarLightGuide%d",iCopyNumber))
{
  ReadGeometryParameters();
  CreateGeometry();
  DefineOpticalSurface();
  SetProperties();
}

void CedarLightGuide2011::ReadGeometryParameters() {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  fPosition         = GeoPars->GetLightGuidePosition();
  fDiameter         = GeoPars->GetLightGuideDiameter();
  fLength           = GeoPars->GetLightGuideDepth(); // = OuterRadius-InnerRadius
  fNConesTotal      = 3;
  fConeInnerRadius  = GeoPars->GetLightGuideConeInnerRadius();
  fConeOuterRadius  = GeoPars->GetLightGuideConeOuterRadius();
  fConeLength       = GeoPars->GetLightGuideConeLength();
  fPMTLength        = GeoPars->GetPMTLength();
}

void CedarLightGuide2011::CreateGeometry() {

  G4double fCylinderRotation = 180*deg; // @@
  G4RotationMatrix* rotCyl = new G4RotationMatrix();
  rotCyl->rotateZ(fCylinderRotation);

  fSolidVolume = new G4Tubs("Cylinder", 0, 0.5*fDiameter, 0.5*fLength, 0*deg, 360*deg);

  fLogicalVolume = new G4LogicalVolume(fSolidVolume, fMaterial, fName);

  fPhysicalVolume = new G4PVPlacement
    (rotCyl, fPosition, fLogicalVolume, fName, fMotherVolume, false, 0);

  ////////////////////////////////////////////////////
  // Cones and PMTs                                 //
  ////////////////////////////////////////////////////

  G4Cons *fCone = new G4Cons
    ("CedarLightGuideCone", 0, fConeOuterRadius, 0, fConeInnerRadius,
     0.5*fConeLength, 0, 360*deg);

  // Material must be the same as of the LG's mother volume
  G4LogicalVolume* ConeLV = new G4LogicalVolume
    (fCone, fMotherVolume->GetMaterial(), "CedarLightGuideCone");

  // Place cones+PMTs inside the LG mother volume
  fConePhysicalVolume = new G4VPhysicalVolume*[fNConesTotal];
  fPMT                = new CedarPMT*[fNConesTotal];

  // place cones and PMTs
  G4double rOffset     = -103*mm;
  G4double rConeCentre =  100*mm;

  G4RotationMatrix* rot = new G4RotationMatrix();
  rot->rotateX(180*deg);

  for (G4int iCone=0; iCone<fNConesTotal; iCone++) {

    G4String fConeName = Form("%s%s%d",fName.data(),"Cone",iCone);
    G4double phi = (iCone-1) * 11.75*deg;

    G4ThreeVector posCone = G4ThreeVector
      (rConeCentre*sin(phi), rConeCentre*cos(phi)+rOffset, 0.5*(fLength-fConeLength));
    G4ThreeVector posPMT = G4ThreeVector
      (rConeCentre*sin(phi), rConeCentre*cos(phi)+rOffset,
       0.5*fLength-fConeLength-0.5*fPMTLength);

      fConePhysicalVolume[iCone] = new G4PVPlacement
	(0, posCone, ConeLV, fConeName, fLogicalVolume, false, iCone);

      fPMT[iCone] = new
	CedarPMT(G4Material::GetMaterial("G4_Al"),
		 fLogicalVolume, iCopyNumber, 0, iCone, rot, posPMT);
  }
}

void CedarLightGuide2011::DefineOpticalSurface() {

  fOpticalSurface = new G4OpticalSurface("CedarLightGuideSurface");
  fOpticalSurface->SetType(dielectric_metal);
  fOpticalSurface->SetModel(glisur);
  fOpticalSurface->SetFinish(polished);
  fOpticalSurface->SetMaterialPropertiesTable
    (CedarMaterialParameters::GetInstance()->GetLightGuideOpticalSurfacePT());

  new G4LogicalSkinSurface("CedarLightGuideSurface",
                           fLogicalVolume, fOpticalSurface);
}
