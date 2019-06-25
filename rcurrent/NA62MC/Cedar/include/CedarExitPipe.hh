// ---------------------------------------------------------------------
// History:
//
// 2014-04-11 Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// ---------------------------------------------------------------------

#ifndef CedarExitPipe_H
#define CedarExitPipe_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "CedarMaterialParameters.hh"
#include "G4Polycone.hh"

#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarExitPipe : public NA62VComponent {

public:
  
  CedarExitPipe(G4Material*, G4LogicalVolume*);
  ~CedarExitPipe();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

private:

  G4double fZLength;
  G4double fInnerRadius1;
  G4double fOuterRadius1;
  G4double fInnerRadius2;
  G4double fOuterRadius2;
  G4ThreeVector fPosition;
};

#endif
