// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2012-04-05
// ---------------------------------------------------------------------

#ifndef CedarAtexCylinder_H
#define CedarAtexCylinder_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "globals.hh"
#include "G4Cons.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarAtexCylinder : public NA62VComponent {

public:

  CedarAtexCylinder(G4Material*, G4LogicalVolume*, G4int);
  ~CedarAtexCylinder() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties() {}

  G4int         GetCopyNumber()                      { return iCopyNumber;    }
  void          SetCopyNumber(G4int value)           { iCopyNumber = value;   }
  G4String      GetName()                            { return fName;          }
  void          SetName(G4String value)              { fName = value;         }
  G4ThreeVector GetPosition()                        { return fPosition;      }
  void          SetPosition(G4ThreeVector value)     { fPosition = value;     }
  G4double      GetMinRadius()                       { return fMinRadius;     }
  void          SetMinRadius(G4double value)         { fMinRadius = value;    }
  G4double      GetMaxRadius()                       { return fMaxRadius;     }
  void          SetMaxRadius(G4double value)         { fMaxRadius = value;    }
  G4double      GetZLength()                         { return fZLength;       }
  void          SetZLength(G4double value)           { fZLength = value;      }
  G4double      GetHoleDiameter()                    { return fHoleDiameter;  }
  void          SetHoleDiameter(G4double value)      { fHoleDiameter = value; }
  G4ThreeVector GetHolePosition()                    { return fHolePosition;  }
  void          SetHolePosition(G4ThreeVector value) { fHolePosition = value; }

private:

  G4int         iCopyNumber;
  G4String      fName;
  G4ThreeVector fPosition;
  G4double      fMinRadius;
  G4double      fMaxRadius;
  G4double      fZLength;
  G4double      fHoleDiameter;
  G4ThreeVector fHolePosition;
};

#endif
