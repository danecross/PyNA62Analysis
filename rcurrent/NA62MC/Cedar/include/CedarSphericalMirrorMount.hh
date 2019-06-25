// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2012-06-15
// ---------------------------------------------------------------------

#ifndef CedarSphericalMirrorMount_H
#define CedarSphericalMirrorMount_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "globals.hh"
#include "G4Polycone.hh"
#include "G4SubtractionSolid.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarSphericalMirrorMount : public NA62VComponent {

public:

  CedarSphericalMirrorMount(G4Material*, G4LogicalVolume*, G4int);
  ~CedarSphericalMirrorMount();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

  G4int         GetCopyNumber()                      { return iCopyNumber;    };
  void          SetCopyNumber(G4int value)           { iCopyNumber = value;   };
  G4String      GetName()                            { return fName;          };
  void          SetName(G4String value)              { fName = value;         };
  G4String      GetName2()                           { return fName2;         };
  void          SetName2(G4String value)             { fName2 = value;        };
  G4ThreeVector GetPosition()                        { return fPosition;      };
  void          SetPosition(G4ThreeVector value)     { fPosition = value;     };
  G4double      GetLength()                          { return fLength;        };
  void          SetLength(G4double value)            { fLength = value;       };
  G4double      GetSupportRingLength()               { return fSupportRingLength;  };
  void          SetSupportRingLength(G4double value) { fSupportRingLength = value; };
  G4double      GetSupportRingRin()                  { return fSupportRingRin;     };
  void          SetSupportRingRin(G4double value)    { fSupportRingRin = value;    };
  G4double      GetSupportRingRout()                 { return fSupportRingRout;    };
  void          SetSupportRingRout(G4double value)   { fSupportRingRout = value;   };
  G4double      GetHoleDiameter()                    { return fHoleDiameter;       };
  void          SetHoleDiameter(G4double value)      { fHoleDiameter = value;      };
  G4double      GetHoleRadialOffset()                { return fHoleRadialOffset;   };
  void          SetHoleRadialOffset(G4double value)  { fHoleRadialOffset = value;  };

private:

  G4int         iCopyNumber;
  G4String      fName;
  G4String      fName2;
  G4ThreeVector fPosition;
  G4double      fLength;
  G4double      fSupportRingLength;
  G4double      fSupportRingRin;
  G4double      fSupportRingRout;
  G4double      fHoleDiameter;
  G4double      fHoleRadialOffset;

};

#endif
