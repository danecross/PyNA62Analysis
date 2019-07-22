// --------------------------------------------------------------
// History:
//
// 2014-03-04 Bob Velghe (bob.velghe@cern.ch)
// - Implementation of MDXH Magnet (TRIM5) 
// 
// --------------------------------------------------------------

#ifndef GigaTrackerMDXMagnet_hh
#define GigaTrackerMDXMagnet_hh 1

#include "NA62VComponent.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerMaterialParameters.hh"
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4FieldManager.hh"
#include "G4UniformMagField.hh"

class GigaTrackerMDXMagnet : public NA62VComponent {

public:

  ~GigaTrackerMDXMagnet();
  GigaTrackerMDXMagnet(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int, G4double);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

private:

  G4ThreeVector fPosition;
  G4int fiCopy;
  G4double fFieldScaleFactor; ///< Run-dependent scale factor to the nominal field strength

  G4double fXLength;
  G4double fYLength;
  G4double fZLength; 
  G4double fFieldXLength;
  G4double fFieldYLength;
  G4double fFieldZLength;
  G4double fGapXLength;
  G4double fGapYLength;
  G4double fGapZLength;

  G4Material * fMagnetCoreMat;

  G4Box * fTopBottomSolidVolume;
  G4LogicalVolume * fTopBottomLogicalVolume;
  G4PVPlacement * fTopPhysicalVolume;
  G4PVPlacement * fBottomPhysicalVolume;

  G4Box * fSideSolidVolume;
  G4LogicalVolume * fSideLogicalVolume;
  G4PVPlacement * fLeftPhysicalVolume;
  G4PVPlacement * fRightPhysicalVolume;

  G4Box * fFieldSolidVolume;
  G4LogicalVolume *fFieldLogicalVolume; 
  G4PVPlacement * fFieldPhysicalVolume; 

  G4ThreeVector fFieldStrength;
  G4FieldManager* fFieldMgr;
  G4UniformMagField * fMagField;

  G4VisAttributes * fVisAttCore;
  G4VisAttributes * fVisAttField;  
};

#endif //GigaTrackerMDXMagnet_hh
