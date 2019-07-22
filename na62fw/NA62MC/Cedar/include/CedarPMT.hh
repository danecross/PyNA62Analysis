// ---------------------------------------------------------------------
// History:
//
// 2012-06-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more precise geometry description
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - compatibility to the general simulation
//
// Created by Karim Massri (karim.massri@cern.ch) 2011-04-28
// ---------------------------------------------------------------------

#ifndef CedarPMT_H
#define CedarPMT_H 1

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

class CedarPMT : public NA62VComponent {

public:
  
  CedarPMT(G4Material*, G4LogicalVolume*, G4int, G4int, G4int, G4RotationMatrix*, G4ThreeVector);
  ~CedarPMT();
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurface();
  void SetProperties();

public:

  G4double GetRadius()                           { return fRadius;                };
  void     SetRadius(G4double value)             { fRadius = value;               };
  G4double GetWindowRadius()                     { return fWindowRadius;          };
  void     SetWindowRadius(G4double value)       { fWindowRadius = value;         };
  G4double GetPhotoCathodeRadius()               { return fPhotoCathodeRadius;    };
  void     SetPhotoCathodeRadius(G4double value) { fPhotoCathodeRadius = value;   };

  G4double GetLength()                           { return fLength;                };
  void	   SetLength(G4double value)             { fLength = value;               };
  G4double GetPreWindowLength()                  { return fPreWindowLength;       };
  void     SetPreWindowLength(G4double value)    { fPreWindowLength = value;      };
  G4double GetWindowLength()                     { return fWindowLength;          };
  void     SetWindowLength(G4double value)       { fWindowLength = value;         };
  G4double GetPhotoCathodeLength()               { return fPhotoCathodeLength;    };
  void     SetPhotoCathodeLength(G4double value) { fPhotoCathodeLength = value;   };

  G4int    GetSector()                           { return fSector;                };
  void     SetSector(G4int value)                { fSector = value;               };
  G4int    GetRow()                              { return fRow;                   };
  void     SetRow(G4int value)                   { fRow = value;                  };
  G4int    GetCone()                             { return fCone;                  };
  void     SetCone(G4int value)                  { fCone = value;                 };
  G4int    GetPositionID()                       { return fPositionID;            };
  void     SetPositionID(G4int value)            { fPositionID = value;           };
  G4int    GetSequentialID()                     { return fSequentialID;          };
  void     SetSequentialID(G4int value)          { fSequentialID = value;         };

  G4ThreeVector GetPosition()                    { return fPosition;              };
  void          SetPosition(G4ThreeVector value) { fPosition = value;             };

  G4VPhysicalVolume* GetWindowPhysicalVolume()   { return fWindowPhysicalVolume;  };
  G4VPhysicalVolume* GetCathodePhysicalVolume()  { return fCathodePhysicalVolume; };

private:

  G4String fName;
  G4String fPreWindowName;
  G4String fWindowName;
  G4String fCathodeName;
  G4int    fSector;
  G4int    fRow;
  G4int    fCone;
  G4int    fPositionID;
  G4int    fSequentialID;
  G4RotationMatrix *fRotation;
  G4ThreeVector fPosition;

  G4double fRadius;
  G4double fWindowRadius;
  G4double fPhotoCathodeRadius;

  G4double fLength;
  G4double fPreWindowLength;
  G4double fWindowLength;
  G4double fPhotoCathodeLength;

  G4LogicalVolume   *fPreWindowLogicalVolume;
  G4LogicalVolume   *fWindowLogicalVolume;
  G4LogicalVolume   *fCathodeLogicalVolume;
  G4VPhysicalVolume *fPreWindowPhysicalVolume;
  G4VPhysicalVolume *fWindowPhysicalVolume;
  G4VPhysicalVolume *fCathodePhysicalVolume;

  G4Material *fPreWindowMaterial;
  G4Material *fWindowMaterial;
  G4Material *fPhotoCathodeMaterial;

  G4OpticalSurface *fOpticalSurface;
};

#endif
