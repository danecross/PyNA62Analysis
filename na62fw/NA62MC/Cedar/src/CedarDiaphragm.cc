// ---------------------------------------------------------------------
// History:
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more realistic geometry
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-16
// ---------------------------------------------------------------------

/// \class CedarDiaphragm
/// \Brief
/// The CEDAR diaphragm
/// \EndBrief

#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "CedarDiaphragm.hh"

#ifdef CEDARRAYTRACING
#include "G4SDManager.hh"
#endif

CedarDiaphragm::CedarDiaphragm
(G4Material* Material, G4LogicalVolume* MotherVolume) :
  NA62VComponent(Material,MotherVolume) {

  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

void CedarDiaphragm::ReadGeometryParameters() {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  iGasH2         = GeoPars->GetGasH2();
  fPosition      = GeoPars->GetDiaphragmPosition();
  fZLength       = GeoPars->GetDiaphragmZLength();
  fInnerRadius   = GeoPars->GetDiaphragmInnerRadius();
  fOuterRadius   = GeoPars->GetMainVesselInnerRadius();
  fOpeningRadius = GeoPars->GetDiaphragmOpeningRadius();
  fApertureR     = GeoPars->GetDiaphragmApertureR();
  fAperturePhi   = GeoPars->GetDiaphragmAperturePhi();
}

#if CEDAR_VIS==0

//*** for tracking ***//

void CedarDiaphragm::CreateGeometry() {

  // The diaphragm disk

  G4Tubs *fSolidDiaphragm = new
    G4Tubs("CedarDiaphragm",
	   fInnerRadius, fOuterRadius, 0.5*fZLength, 0, 360*deg);

  G4LogicalVolume *fLogicalDiaphragm = new G4LogicalVolume
    (fSolidDiaphragm, fMaterial, "CedarDiaphragm");

  new G4PVPlacement(0, fPosition, fLogicalDiaphragm,
		    "CedarDiaphragm", fMotherVolume, false, 0);

#ifdef CEDARRAYTRACING
  // Connect Diaphragm volume to CedarSD
  G4SDManager* fSDmanager = G4SDManager::GetSDMpointer();
  G4String fCedarSensitiveDetectorName = "/Cedar";
  G4VSensitiveDetector* fCedarSD =
    fSDmanager->FindSensitiveDetector(fCedarSensitiveDetectorName);
  fLogicalDiaphragm->SetSensitiveDetector(fCedarSD);
#endif

  // The gaps in the disk

  const G4int fNSectors =
    CedarGeometryParameters::GetInstance()->GetNSectors();

  fSolidGap   = new G4Tubs*[fNSectors];
  fLogicalGap = new G4LogicalVolume*[fNSectors];

  for (G4int iSector=0; iSector<fNSectors; iSector++) {

    G4String name = Form("CedarDiaphragmGap%d", iSector+1);
    G4double fCentralAngle = (iSector+2.5) * 360*deg / fNSectors;

    fSolidGap[iSector] = new
      G4Tubs(name,
	     fOpeningRadius-0.5*fApertureR, fOpeningRadius+0.5*fApertureR,
	     0.5*fZLength,
	     fCentralAngle-0.5*fAperturePhi, fAperturePhi);

    G4Material *fCedarRadiatorGas = (iGasH2) ?
      G4Material::GetMaterial("Cedar_RadiatorHydrogen") :
      G4Material::GetMaterial("Cedar_RadiatorNitrogen");

    fLogicalGap[iSector] = new
      G4LogicalVolume(fSolidGap[iSector], fCedarRadiatorGas, name);

    new G4PVPlacement(0, G4ThreeVector(0,0,0),
		      fLogicalGap[iSector],
		      name, fLogicalDiaphragm, false, iSector);

#ifdef CEDARRAYTRACING
    fLogicalGap[iSector]->SetSensitiveDetector(fCedarSD);
#endif
  }
}

#else

//*** for visualisation ***//

void CedarDiaphragm::CreateGeometry() {

  G4Tubs *fTube1 = new
    G4Tubs("CedarDiaphragm1",
           fInnerRadius, fOpeningRadius-0.5*fApertureR,
           0.5*fZLength, 0, 360*deg);

  G4Tubs *fTube2 = new
    G4Tubs("CedarDiaphragm2",
           fOpeningRadius+0.5*fApertureR, fOuterRadius,
           fZLength, 0, 360*deg);

  G4LogicalVolume *fLogicalVolume1 = new
    G4LogicalVolume(fTube1, fMaterial, "CedarDiaphragm1", 0, 0, 0);

  G4LogicalVolume *fLogicalVolume2 = new
    G4LogicalVolume(fTube2, fMaterial, "CedarDiaphragm2", 0, 0, 0);

  new G4PVPlacement(0, fPosition, fLogicalVolume1,
                    "CedarDiaphragm1", fMotherVolume, false, 0);

  new G4PVPlacement(0, fPosition, fLogicalVolume2,
                    "CedarDiaphragm2", fMotherVolume, false, 0);
}

#endif
