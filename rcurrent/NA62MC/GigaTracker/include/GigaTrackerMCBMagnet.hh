// --------------------------------------------------------------
// History:
//
// 2014-03-04 Bob Velghe (bob.velghe@cern.ch)
// - Implementation of MCBV Magnets (Archomats) 
// 
// --------------------------------------------------------------

#ifndef GigaTrackerMCBMagnet_hh
#define GigaTrackerMCBMagnet_hh 1

#include "NA62VComponent.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerMaterialParameters.hh"

#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4FieldManager.hh"
#include "G4UniformMagField.hh"

class GigaTrackerMCBMagnet : public NA62VComponent {

public:

  GigaTrackerMCBMagnet(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int, G4int, G4double);
  ~GigaTrackerMCBMagnet() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

private:

  G4ThreeVector fPosition;

  G4int fiCopy;
  G4int fOrientation;         ///< Yoke orientation: 0 = normal orientation; 1 = yoke up
  G4double fFieldScaleFactor; ///< Run-dependent scale factor to the nominal field strength

  G4double fXLength;
  G4double fYLength;
  G4double fZLength;
  G4double fFieldXLength;
  G4double fFieldYLength;
  G4double fFieldZLength;

  G4double fBaseYLength;
  G4double fGapXLength;
  G4double fSideYLength;
  G4double fHatYLength;
  G4double fBeamYPos;

  G4Material * fMagnetCoreMat;

  G4Box * fBaseSolidVolume;
  G4LogicalVolume * fBaseLogicalVolume;
  G4PVPlacement * fBasePhysicalVolume;

  G4Box * fSideSolidVolume;
  G4LogicalVolume * fSideLogicalVolume;
  G4PVPlacement * fLeftSidePhysicalVolume;
  G4PVPlacement * fRightSidePhysicalVolume;

  G4Box * fHatSolidVolume;
  G4LogicalVolume * fHatLogicalVolume;
  G4PVPlacement * fLeftHatPhysicalVolume;
  G4PVPlacement * fRightHatPhysicalVolume;

  G4Box * fFieldSolidVolume;
  G4LogicalVolume *fFieldLogicalVolume; 
  G4PVPlacement * fFieldPhysicalVolume; 

  G4ThreeVector fFieldStrength;
  G4FieldManager* fFieldMgr;
  G4UniformMagField * fMagField;

  G4VisAttributes * fVisAttCore;
  G4VisAttributes * fVisAttField;  
};

#endif //GigaTrackerMCBMagnet_hh
