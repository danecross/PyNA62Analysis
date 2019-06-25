// ---------------------------------------------------------------------
// History:
//
// 2011-06-10 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more detailed description of the vessel shape
//
// 2009-11-16 Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// ---------------------------------------------------------------------

/// \class CedarVessel
/// \Brief
/// Build the gas-filled Cedar vessel, quartz windows, beam pipes, vacuum windows
/// \EndBrief

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "CedarVessel.hh"
#include "CedarMaterialParameters.hh"
#include "G4LogicalSkinSurface.hh"
#include "G4SubtractionSolid.hh"

#ifdef CEDARRAYTRACING
#include "G4SDManager.hh"
#endif

CedarVessel::CedarVessel(G4Material *Material, G4LogicalVolume *MotherVolume) : 
  NA62VComponent(Material,MotherVolume) {
  ReadGeometryParameters();
  CedarMaterialParameters::GetInstance();
  CreateGeometry();
  DefineOpticalSurface();
  SetProperties();
}

void CedarVessel::ReadGeometryParameters() {

  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();

  iGasH2                      = GeoPars->GetGasH2();
  fQuartzWindowRadialOffset   = GeoPars->GetQuartzWindowRadialOffset();
  fQuartzWindowRadius         = GeoPars->GetQuartzWindowRadius();

  fEntranceWindowZLength      = GeoPars->GetEntranceWindowZLength();
  fFrontPipeZLength           = GeoPars->GetFrontPipeZLength();
  fFrontVesselZLength         = GeoPars->GetFrontVesselZLength();
  fMainVesselCylinderZLength  = GeoPars->GetMainVesselCylinderZLength();
  fExitWindowZLength          = GeoPars->GetExitWindowZLength();

  fFrontPipeInnerRadius       = GeoPars->GetFrontPipeInnerRadius();
  fFrontPipeOuterRadius       = GeoPars->GetFrontPipeOuterRadius();
  fFrontVesselInnerRadius     = GeoPars->GetFrontVesselInnerRadius();
  fFrontVesselOuterRadius     = GeoPars->GetFrontVesselOuterRadius();
  fMainVesselInnerRadius      = GeoPars->GetMainVesselInnerRadius();
  fMainVesselOuterRadius      = GeoPars->GetMainVesselOuterRadius();
  fExitPipeInnerRadius1       = GeoPars->GetExitPipeInnerRadius1();
  fExitPipeOuterRadius1       = GeoPars->GetExitPipeOuterRadius1();

  fEntranceWindowPosition     = GeoPars->GetEntranceWindowPosition();
  fFrontPipePosition          = GeoPars->GetFrontPipePosition();
  fQuartzWindowDiskPosition   = GeoPars->GetQuartzWindowDiskPosition();
  fFrontVesselPosition        = GeoPars->GetFrontVesselPosition();
  fMainVesselCylinderPosition = GeoPars->GetMainVesselCylinderPosition();
  fExitWindowPosition         = GeoPars->GetExitWindowPosition();

  fQuartzWindowZLength        = GeoPars->GetQuartzWindowZLength();
  fFrontVesselZLength         = GeoPars->GetFrontVesselZLength();
  fMainVesselCylinderZLength  = GeoPars->GetMainVesselCylinderZLength();
}

void CedarVessel::CreateGeometry() {

  const G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();

  fQuartzWindowLogicalVolume  = new G4LogicalVolume*[fNSectors];
  fQuartzWindowPhysicalVolume = new G4VPhysicalVolume*[fNSectors];

  /** Place front, main, endcap vessel sections **/

  const G4int nPlanes = 7;
  G4double z[nPlanes], rIn[nPlanes], rOut[nPlanes], rZero[nPlanes];

  z[0] = fFrontVesselPosition.z() - 0.5*fFrontVesselZLength;
  z[1] = z[0] + fFrontVesselZLength;
  z[2] = z[1];
  z[3] = z[2] + 1.0*cm;
  z[4] = z[3];
  z[5] = fMainVesselCylinderPosition.z() + 0.5*fMainVesselCylinderZLength;
  z[6] = fExitWindowPosition.z()-0.5*fExitWindowZLength;

  rIn[0] = fFrontVesselInnerRadius;
  rIn[1] = rIn[0];
  rIn[2] = rIn[1];
  rIn[3] = rIn[2];
  rIn[4] = fMainVesselInnerRadius;
  rIn[5] = rIn[4];
  rIn[6] = fExitPipeInnerRadius1;

  rOut[0] = fFrontVesselOuterRadius;
  rOut[1] = rOut[0];
  rOut[2] = fMainVesselOuterRadius;
  rOut[3] = rOut[2];
  rOut[4] = rOut[3];
  rOut[5] = rOut[4];
  rOut[6] = fExitPipeOuterRadius1;

  for (G4int i=0; i<nPlanes; i++) rZero[i] = 0;

  G4Polycone *fSolidCedarVessel = new G4Polycone
    ("CedarVessel", 0*deg, 360*deg, nPlanes, z, rIn, rOut);

  G4LogicalVolume *fLogicCedarVessel = new G4LogicalVolume
    (fSolidCedarVessel, fMaterial, "CedarVessel", 0, 0, 0);

  new G4PVPlacement
    (0, G4ThreeVector(0,0,0), fLogicCedarVessel,
     "CedarVessel", fMotherVolume, false, 0);

  G4Tubs *fSolidQuartzWindow = new G4Tubs
    ("QuartzWindow", 0, fQuartzWindowRadius, 0.5*fQuartzWindowZLength, 0, 360*deg);

#if CEDAR_VIS==1

  /*******************************************************************/
  /* QUARTZ WINDOWS AND DISK: FOR DRAWING (USING VOLUME SUBTRACTION) */
  /*******************************************************************/

  /** Place the quartz windows **/
  
  G4Tubs *fSolidQuartzWindowToSubtract = new // it is thicker
    G4Tubs("QuartzWindow", 0, fQuartzWindowRadius, 0.6*fQuartzWindowZLength,
	   0*deg, 360*deg);

  for (G4int iWindow=0; iWindow<fNSectors; iWindow++) {
    fQuartzWindowLogicalVolume[iWindow] = new G4LogicalVolume
      (fSolidQuartzWindow,
       G4Material::GetMaterial("Cedar_QuartzWindowQuartz1"),
       "CedarQuartzWindow");
  }

  G4Tubs *fInitialQuartzWindowDisk = new G4Tubs
    ("QuartzWindowDisk",
     fFrontPipeInnerRadius, fFrontVesselOuterRadius, 0.5*fQuartzWindowZLength,
     0*deg, 360*deg);

  G4SubtractionSolid **fQuartzWindowDisk = new G4SubtractionSolid*[fNSectors];

  for (G4int iWindow=0; iWindow<fNSectors; iWindow++) {

    G4double angle      = (iWindow+2.5) * 360*deg / fNSectors;
    G4double x          = fQuartzWindowRadialOffset * cos(angle);
    G4double y          = fQuartzWindowRadialOffset * sin(angle);
    G4ThreeVector trans = G4ThreeVector(x,y,0);
    G4ThreeVector pos   = G4ThreeVector(x,y,fQuartzWindowDiskPosition.z());

    if (!iWindow) {
      fQuartzWindowDisk[iWindow] = new
	G4SubtractionSolid ("sub", fInitialQuartzWindowDisk,
			    fSolidQuartzWindowToSubtract,
			    0, trans);
    }
    else {
      fQuartzWindowDisk[iWindow] = new
        G4SubtractionSolid ("sub", fQuartzWindowDisk[iWindow-1],
			    fSolidQuartzWindowToSubtract,
			    0, trans);
    }

    G4String name = Form("CedarQuartzWindow%d", iWindow+1);
    fQuartzWindowPhysicalVolume[iWindow] = new G4PVPlacement
      (0, pos, fQuartzWindowLogicalVolume[iWindow], name, fMotherVolume, false, iWindow);
  }

  /** Place the disk holding the quartz windows **/
  G4LogicalVolume *fLogicQuartzWindowDisk = new G4LogicalVolume
    (fQuartzWindowDisk[fNSectors-1], fMaterial, "CedarQWDisk");

  new G4PVPlacement
    (0, fQuartzWindowDiskPosition, fLogicQuartzWindowDisk,
     "CedarQWDisk", fMotherVolume, false, 0);

#else

  /*******************************************************************/
  /* QUARTZ WINDOWS AND DISK: FOR TRACKING (USING DAUGHTER VOLUMES)  */
  /*******************************************************************/

  /** Place the disk holding the quartz windows **/

  G4Tubs *fQuartzWindowDisk = new G4Tubs
    ("CedarQWDisk",
     fFrontPipeInnerRadius, fFrontVesselOuterRadius, 0.5*fQuartzWindowZLength,
     0*deg, 360*deg);

  G4LogicalVolume *fLogicQuartzWindowDisk = new G4LogicalVolume
    (fQuartzWindowDisk, fMaterial, "CedarQWDisk");

  new G4PVPlacement
    (0, fQuartzWindowDiskPosition, fLogicQuartzWindowDisk,
     "CedarQWDisk", fMotherVolume, false, 0);

  /** Place the quartz windows **/

  for (G4int iWindow=0; iWindow<fNSectors; iWindow++) {

    G4double angle    = (iWindow+2.5) * 360*deg / fNSectors;
    G4double x        = fQuartzWindowRadialOffset * cos(angle);
    G4double y        = fQuartzWindowRadialOffset * sin(angle);
    G4ThreeVector pos = G4ThreeVector(x,y,0);

    G4String MaterialName = Form("Cedar_QuartzWindowQuartz%d", iWindow+1);
    G4String Name         = Form("CedarQuartzWindow%d", iWindow+1);

    fQuartzWindowLogicalVolume[iWindow] = new G4LogicalVolume
      (fSolidQuartzWindow, G4Material::GetMaterial(MaterialName), Name);

    fQuartzWindowPhysicalVolume[iWindow] = new G4PVPlacement
      (0, pos, fQuartzWindowLogicalVolume[iWindow], Name,
       fLogicQuartzWindowDisk, false, iWindow);

#ifdef CEDARRAYTRACING
    G4SDManager* fSDmanager = G4SDManager::GetSDMpointer();
    G4String fCedarSensitiveDetectorName = "/Cedar";
    G4VSensitiveDetector* fCedarSD =
      fSDmanager->FindSensitiveDetector(fCedarSensitiveDetectorName);
    fQuartzWindowLogicalVolume[iWindow]->SetSensitiveDetector(fCedarSD);
#endif
  }

#endif

  /*********************************************************************/

  /** Place the front beam pipe **/

  G4Tubs *fSolidFrontPipe = new G4Tubs
    ("CedarFrontPipe", fFrontPipeInnerRadius, fFrontPipeOuterRadius,
     0.5*fFrontPipeZLength, 0*deg, 360*deg);

  G4LogicalVolume *fLogicFrontPipe = new G4LogicalVolume
    (fSolidFrontPipe, fMaterial, "CedarFrontPipe");

  new G4PVPlacement
    (0, fFrontPipePosition, fLogicFrontPipe,
     "CedarFrontPipe", fMotherVolume, false, 0);

  /** Place the vacuum windows **/

  G4Tubs *fSolidEntranceWindow = new G4Tubs
    ("CedarEntranceWindow", 0, fFrontPipeOuterRadius,
     0.5*fEntranceWindowZLength, 0*deg, 360*deg);

  G4LogicalVolume *fLogicEntranceWindow = new G4LogicalVolume
    (fSolidEntranceWindow, G4Material::GetMaterial("G4_Al"), "CedarEntranceWindow");

  new G4PVPlacement
    (0, fEntranceWindowPosition, fLogicEntranceWindow,
     "CedarEntranceWindow", fMotherVolume, false, 0);

  G4Tubs *fSolidExitWindow = new G4Tubs
    ("CedarExitWindow", 0, fExitPipeOuterRadius1, 0.5*fExitWindowZLength,
     0*deg, 360*deg);

  G4LogicalVolume *fLogicExitWindow = new G4LogicalVolume
    (fSolidExitWindow, G4Material::GetMaterial("G4_Al"), "CedarExitWindow");

  new G4PVPlacement
    (0, fExitWindowPosition, fLogicExitWindow,
     "CedarExitWindow", fMotherVolume, false, 0);

  /** Fill the vessel with the radiator gas **/

  G4Material *fCedarRadiatorGas = (iGasH2) ?
    G4Material::GetMaterial("Cedar_RadiatorHydrogen") :
    G4Material::GetMaterial("Cedar_RadiatorNitrogen");

  // ... in the front pipe
  G4Tubs *fSolidRadiatorGasFrontPipe = new G4Tubs
    ("CedarRadiatorGasFrontPipe",
     0, fFrontPipeInnerRadius, 0.5*(fFrontPipeZLength+fQuartzWindowZLength),
     0*deg, 360*deg);

  fFrontPipeRadiatorGasLogicalVolume = new G4LogicalVolume
    (fSolidRadiatorGasFrontPipe, fCedarRadiatorGas, "CedarFrontPipeRadiatorGas");

  G4ThreeVector pos = G4ThreeVector(0, 0, 0.5*fQuartzWindowZLength);
  fFrontPipeRadiatorGasPhysicalVolume = new G4PVPlacement
    (0, fFrontPipePosition + pos,
     fFrontPipeRadiatorGasLogicalVolume, "CedarFrontPipeRadiatorGas",
     fMotherVolume, false, 0);

  // ... in the main vessel
  G4Polycone *fSolidCedarVesselRadiatorGas = new G4Polycone
    ("CedarMainVesselraduatorGas", 0*deg, 360*deg, nPlanes, z, rZero, rIn);

  fVesselRadiatorGasLogicalVolume = new G4LogicalVolume
    (fSolidCedarVesselRadiatorGas, fCedarRadiatorGas, "CedarRadiatorGas");

  fVesselRadiatorGasPhysicalVolume = new G4PVPlacement
    (0, G4ThreeVector(0,0,0), fVesselRadiatorGasLogicalVolume, "CedarRadiatorGas",
     fMotherVolume, false, 0);
}

void CedarVessel::DefineOpticalSurface() {
  if (!CedarMaterialParameters::GetInstance()->OpticalPropertiesEnabled()) return;

  // Ideal quartz window surface: no reflections.
  // All losses are modelled by the transmittance.

  fQuartzWindowOpticalSurface = new G4OpticalSurface("CedarQuartzWindowOpticalSurface");
  fQuartzWindowOpticalSurface->SetType(dielectric_dielectric);
  fQuartzWindowOpticalSurface->SetModel(glisur);
  fQuartzWindowOpticalSurface->SetFinish(polished);
  fQuartzWindowOpticalSurface->SetMaterialPropertiesTable
    (CedarMaterialParameters::GetInstance()->GetLensOpticalSurfacePT());

  const G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();
  for (int iWindow=0; iWindow<fNSectors; iWindow++) {
    G4String fName = Form("CedarQuartzWindowOpticalSurface%d", iWindow+1);
    new G4LogicalSkinSurface(fName, fQuartzWindowLogicalVolume[iWindow],
			     fQuartzWindowOpticalSurface);
  }
}

void CedarVessel::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(0.5,0.5,0.5));
}
