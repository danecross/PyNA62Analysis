#ifndef HACMagnet_H
#define HACMagnet_H 1

#include "NA62VComponent.hh"
#include "HACGeometryParameters.hh"
#include "globals.hh"

#include "G4FieldManager.hh"
#include "G4UniformMagField.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;

class HACMagnet : public NA62VComponent
{

public:
  
  ~HACMagnet();
  HACMagnet(G4Material*, G4LogicalVolume*, G4ThreeVector);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

private:

  G4ThreeVector fPosition;
  G4double fZLength;
  G4double fXLength;
  G4double fYLength;

  G4FieldManager* fFieldMgr;
  G4UniformMagField * fMagField;
  G4double fFieldStrength;
};

#endif
