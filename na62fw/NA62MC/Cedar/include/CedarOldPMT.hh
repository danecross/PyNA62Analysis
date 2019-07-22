// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2012-02-16
// ---------------------------------------------------------------------

#ifndef CedarOldPMT_H
#define CedarOldPMT_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "G4Tubs.hh"
#include "globals.hh"
#include "G4OpticalSurface.hh"
#include "G4LogicalSkinSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarOldPMT : public NA62VComponent {

public:
  
  CedarOldPMT(G4Material*, G4LogicalVolume*, G4int);
  ~CedarOldPMT() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurface();
  void SetProperties();

public:

  G4double GetLength()                           { return fLength;                }
  void	   SetLength(G4double value)             { fLength = value;               }
  G4double GetRadius()                           { return fRadius;                }
  void     SetRadius(G4double value)             { fRadius = value;               }
  G4double GetWindowRadius()                     { return fWindowRadius;          }
  void     SetWindowRadius(G4double value)       { fWindowRadius = value;         }
  G4double GetWindowLength()                     { return fWindowLength;          }
  void     SetWindowLength(G4double value)       { fWindowLength = value;         }
  G4double GetPhotoCathodeRadius()               { return fPhotoCathodeRadius;    }
  void     SetPhotoCathodeRadius(G4double value) { fPhotoCathodeRadius = value;   }
  G4double GetPhotoCathodeLength()               { return fPhotoCathodeLength;    }
  void     SetPhotoCathodeLength(G4double value) { fPhotoCathodeLength = value;   }

  G4ThreeVector GetPosition()                    { return fPosition;              }
  void          SetPosition(G4ThreeVector value) { fPosition = value;             }

  G4VPhysicalVolume* GetWindowPhysicalVolume()   { return fWindowPhysicalVolume;  }
  G4VPhysicalVolume* GetCathodePhysicalVolume()  { return fCathodePhysicalVolume; }

private:

  G4String fName;
  G4String fWindowName;
  G4String fCathodeName;
  G4int    fSector;
  G4int    fCopyNumber;
  G4ThreeVector fPosition;

  G4double fLength;
  G4double fRadius;
  G4double fWindowRadius;
  G4double fWindowLength;
  G4double fPhotoCathodeRadius;
  G4double fPhotoCathodeLength;

  G4LogicalVolume* fWindowLogicalVolume;
  G4LogicalVolume* fCathodeLogicalVolume;
  G4VPhysicalVolume *fWindowPhysicalVolume;
  G4VPhysicalVolume *fCathodePhysicalVolume;

  G4Material*      fWindowMaterial;
  G4Material*      fPhotoCathodeMaterial;

  G4OpticalSurface *fOpticalSurface;
};

#endif
