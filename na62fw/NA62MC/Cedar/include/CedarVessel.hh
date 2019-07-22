// ---------------------------------------------------------------------
// History:
//
// 2011-06-10 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more detailed description of the vessel shape
//
// 2009-11-16 Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// ---------------------------------------------------------------------

#ifndef CedarVessel_H
#define CedarVessel_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "CedarMaterialParameters.hh"
#include "G4Tubs.hh"
#include "G4Box.hh"
#include "G4Polycone.hh"
#include "G4Sphere.hh"

//#include "globals.hh"

#include "G4OpticalSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarVessel : public NA62VComponent {

public:
  
  CedarVessel(G4Material*, G4LogicalVolume*);
  ~CedarVessel() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurface();
  void SetProperties();

  G4LogicalVolume *GetFrontPipeRadiatorGasLogicalVolume()
  { return fFrontPipeRadiatorGasLogicalVolume; }
  G4LogicalVolume *GetVesselRadiatorGasLogicalVolume()
  { return fVesselRadiatorGasLogicalVolume; }
  G4VPhysicalVolume *GetFrontPipeRadiatorGasPhysicalVolume()
  { return fFrontPipeRadiatorGasPhysicalVolume; }
  G4VPhysicalVolume *GetVesselRadiatorGasPhysicalVolume()
  { return fVesselRadiatorGasPhysicalVolume; }

private:

  G4int    iGasH2;
  G4double fQuartzWindowRadialOffset;
  G4double fQuartzWindowRadius;
  G4double fQuartzWindowZLength;

  G4double fEntranceWindowZLength;
  G4double fFrontPipeZLength;
  G4double fFrontVesselZLength;
  G4double fMainVesselCylinderZLength;
  G4double fExitWindowZLength;

  G4double fFrontPipeInnerRadius;
  G4double fFrontPipeOuterRadius;
  G4double fFrontVesselInnerRadius;
  G4double fFrontVesselOuterRadius;
  G4double fMainVesselInnerRadius;
  G4double fMainVesselOuterRadius;
  G4double fExitPipeInnerRadius1;
  G4double fExitPipeOuterRadius1;

  G4ThreeVector fEntranceWindowPosition;
  G4ThreeVector fFrontPipePosition;
  G4ThreeVector fQuartzWindowDiskPosition;
  G4ThreeVector fFrontVesselPosition;
  G4ThreeVector fMainVesselCylinderPosition;
  G4ThreeVector fExitWindowPosition;

  G4LogicalVolume   *fFrontPipeRadiatorGasLogicalVolume;
  G4LogicalVolume   *fVesselRadiatorGasLogicalVolume;
  G4LogicalVolume   **fQuartzWindowLogicalVolume;
  G4VPhysicalVolume *fFrontPipeRadiatorGasPhysicalVolume;
  G4VPhysicalVolume *fVesselRadiatorGasPhysicalVolume;

  G4VPhysicalVolume **fQuartzWindowPhysicalVolume;
  G4OpticalSurface  *fQuartzWindowOpticalSurface;
};

#endif
