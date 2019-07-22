#ifndef HACDetector_H
#define HACDetector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "HACGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class HACDetector : public NA62VComponent, public NA62VNamed
{

public:
  
  ~HACDetector();
  HACDetector(G4Material*, G4LogicalVolume*);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

private:

  G4double fHACRespRegionZCenter;
  G4double fHACRespRegionXLength;
  G4double fHACRespRegionYLength;
  G4double fHACRespRegionZLength;
  G4double fMagnetPosZ;
  G4double fHACDetectorYRotation;
  G4double fHACModuleXPosition[9];
  G4double fHACModuleYPosition[9];
  G4double fHACModuleZPosition;

};

#endif
