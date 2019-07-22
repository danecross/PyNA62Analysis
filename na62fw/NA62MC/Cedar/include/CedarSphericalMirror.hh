// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-13
// ---------------------------------------------------------------------

#ifndef CedarSphericalMirror_H
#define CedarSphericalMirror_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "CedarMaterialParameters.hh"
#include "globals.hh"

#include "G4OpticalSurface.hh"
#include "G4LogicalSkinSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarSphericalMirror : public NA62VComponent {

public:
  
  CedarSphericalMirror(G4Material*, G4LogicalVolume*, G4int);
  ~CedarSphericalMirror() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurface();
  void SetProperties();

  G4int         GetSectorNumber()                { return iSector;         }
  void          SetSectorNumber(G4int value)     { iSector = value;        }
  G4String      GetName()                        { return fName;           }
  void          SetName(G4String value)          { fName = value;          }
  G4ThreeVector GetPosition()                    { return fPosition;       }
  void          SetPosition(G4ThreeVector value) { fPosition = value;      }
  G4double      GetSurfaceRadius()               { return fSurfaceRadius;  }
  void          SetSurfaceRadius(G4double value) { fSurfaceRadius = value; }
  G4double      GetDiameter()                    { return fDiameter;       }
  void          SetDiameter(G4double value)      { fDiameter = value;      }
  G4double      GetCentralAngle()                { return fCentralAngle;   }
  void          SetCentralAngle(G4double value)  { fCentralAngle = value;  }
  G4double      GetOpeningAngle()                { return fOpeningAngle;   }
  void          SetOpeningAngle(G4double value)  { fOpeningAngle = value;  }

private:

  G4int         iSector;
  G4String      fName;
  G4ThreeVector fPosition;
  G4double      fSurfaceRadius;
  G4double      fDiameter;
  G4double      fCentralAngle;
  G4double      fOpeningAngle;

  G4OpticalSurface* fOpticalSurface;
  G4LogicalSkinSurface* fLogicalSkinSurface;
};

#endif
