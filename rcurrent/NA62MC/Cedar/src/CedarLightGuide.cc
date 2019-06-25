// ---------------------------------------------------------------------
// History:
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - cones and PMTs added
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-13
// ---------------------------------------------------------------------

/// \class CedarLightGuide
/// \Brief
/// CEDAR lightguide and PMT array
/// \EndBrief

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "CedarLightGuide.hh"
#include "G4Sphere.hh"
#include "G4IntersectionSolid.hh"

#ifdef CEDARRAYTRACING
#include "G4SDManager.hh"
#endif

CedarLightGuide::CedarLightGuide
(G4Material* Material, G4LogicalVolume* MotherVolume, G4int iSector) :
  NA62VComponent(Material,MotherVolume),
  iCopyNumber(iSector), fName(Form("CedarLightGuide%d", iSector+1)) {
  ReadGeometryParameters();
  CreateGeometry();
  if (iLGType>0) DefineOpticalSurface();
  SetProperties();
}

void CedarLightGuide::ReadGeometryParameters() {

  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();

  // The lightguide itself
  iLGType           = GeoPars->GetLightGuideType();
  fPosition         = GeoPars->GetLightGuidePosition();
  fInnerRadius      = GeoPars->GetLightGuideInnerRadius();
  fOuterRadius      = GeoPars->GetLightGuideOuterRadius();
  fDiameter         = GeoPars->GetLightGuideDiameter();

  // Cones and PMTs
  fNConesTotal      = GeoPars->GetLightGuideNConesTotal();
  fNofRows          = GeoPars->GetLightGuideNofRows();
  fNofCones         = GeoPars->GetLightGuideNofCones();     // array: per row
  fInstrumented     = GeoPars->GetLightGuideInstrumented(); // array: per row
  fRowsPhiShift     = GeoPars->GetLightGuideRowsPhiShift(); // array: per row
  fConesPhiShift    = GeoPars->GetLightGuideConesPhiShift();
  fConesThetaShift  = GeoPars->GetLightGuideConesThetaShift();
  fConeInnerRadius  = GeoPars->GetLightGuideConeInnerRadius();
  fConeOuterRadius  = GeoPars->GetLightGuideConeOuterRadius();
  fConeLength       = GeoPars->GetLightGuideConeLength();
  fPMTLength        = GeoPars->GetPMTLength();
}

void CedarLightGuide::CreateGeometry() {

  G4Sphere *fSolidSphere = new G4Sphere
    ("sphere", fInnerRadius, fOuterRadius, 0, 360*deg, 0, 180*deg);

  G4Tubs *fSolidTubs = new G4Tubs
    ("tubs", 0, 0.5*fDiameter, 0.5*fOuterRadius, 0*deg, 360*deg);

  G4ThreeVector trans(0, 0.5*fOuterRadius, 0);

  G4RotationMatrix* rot = new G4RotationMatrix();
  rot->rotateX(90*deg);

  G4IntersectionSolid *fSolid = new G4IntersectionSolid
    (fName, fSolidSphere, fSolidTubs, rot, trans);

  fLogicalVolume = new G4LogicalVolume(fSolid, fMaterial, fName);

  fPhysicalVolume = new G4PVPlacement
    (0, fPosition, fLogicalVolume, fName, fMotherVolume, false, 0);

  /////////////////
  // Cones and PMTs

  G4Cons *fConeRaw = new
    G4Cons("CedarLightGuideConeRaw",
	   0, fConeInnerRadius, 0, fConeOuterRadius, 0.5*fConeLength,
	   0, 360*deg);

  // correction to the cone length due to the curved surface
  G4double dx = fInnerRadius*(1-cos(asin(fConeInnerRadius/fInnerRadius)));

  // Cone centre radius
  G4double rConeCentre = fInnerRadius+0.5*fConeLength-dx;

  // PMT centre radius
  G4double rPMTCentre = rConeCentre+0.5*(fConeLength+fPMTLength);

  G4ThreeVector transCone(0, 0, -rConeCentre);
  G4IntersectionSolid *fCone = new G4IntersectionSolid
    ("CedarLightGuideCone", fConeRaw, fSolidSphere, 0, transCone);

  // Material must be the same as of the LG's mother volume
  G4LogicalVolume* ConeLV = new G4LogicalVolume
    (fCone, fMotherVolume->GetMaterial(), "CedarLightGuideCone");

  // Place cones+PMTs inside the LG mother volume
  fConePhysicalVolume = new G4VPhysicalVolume*[fNConesTotal];
  fPMT                = new CedarPMT*[fNConesTotal];

  // Place the cones
  G4int indexCone = 0, indexPMT = 0;
  for (G4int iRow=0; iRow<fNofRows; iRow++) {

    G4double theta  = (iRow-0.5*(fNofRows-1))*fConesThetaShift + 90*deg;
    G4double phiRow = 0.5*fConesPhiShift*fRowsPhiShift[iRow];

    for (G4int iCone=0; iCone<fNofCones[iRow]; iCone++) {

      G4double phi = -(iCone-0.5*(fNofCones[iRow]-1))*fConesPhiShift - phiRow;
      G4RotationMatrix* rot2 = new G4RotationMatrix();
      rot2->rotateZ(phi);
      rot2->rotateX(theta);

      G4ThreeVector posCone = G4ThreeVector
	(rConeCentre*sin(theta)*sin(phi),
	 rConeCentre*sin(theta)*cos(phi),
	 rConeCentre*cos(theta));

      G4int PositionID = 111 + 100*iCopyNumber + 10*iRow + iCone + 2*(iRow==0);
      fConePhysicalVolume[indexCone] = new G4PVPlacement
	(rot2, posCone, ConeLV, Form("CedarCone%03d", PositionID),
	 fLogicalVolume, false, indexCone);

      indexCone++;

      // place a PMT
      if ((fInstrumented[iRow]>>iCone)&1) { // is this cone instrumented?
	G4ThreeVector posPMT = G4ThreeVector
	  (rPMTCentre*sin(theta)*sin(phi),
	   rPMTCentre*sin(theta)*cos(phi),
	   rPMTCentre*cos(theta));
	fPMT[indexPMT] = new
	  CedarPMT(G4Material::GetMaterial("G4_Al"),
		   fLogicalVolume, iCopyNumber, iRow, iCone, rot2, posPMT);
	indexPMT++;
      }
    }
  }

#ifdef CEDARRAYTRACING
  G4SDManager* fSDmanager = G4SDManager::GetSDMpointer();
  G4String fCedarSensitiveDetectorName = "/Cedar";
  G4VSensitiveDetector* fCedarSD =
    fSDmanager->FindSensitiveDetector(fCedarSensitiveDetectorName);
  fLogicalVolume->SetSensitiveDetector(fCedarSD);
  ConeLV->SetSensitiveDetector(fCedarSD);
#endif
}

void CedarLightGuide::DefineOpticalSurface() {

  if (!CedarMaterialParameters::GetInstance()->OpticalPropertiesEnabled()) return;

  // Light guide reflecting surface
  fOpticalSurface = new G4OpticalSurface("CedarLightGuideSurface");
  fOpticalSurface->SetType(dielectric_metal);
  fOpticalSurface->SetModel(glisur);
  fOpticalSurface->SetFinish(polished);
  fOpticalSurface->SetMaterialPropertiesTable
    (CedarMaterialParameters::GetInstance()->GetLightGuideOpticalSurfacePT());
  new G4LogicalSkinSurface("CedarLightGuideSurface",
                           fLogicalVolume, fOpticalSurface);
}
