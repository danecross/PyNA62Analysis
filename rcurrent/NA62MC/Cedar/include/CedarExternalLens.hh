// ---------------------------------------------------------------------
// History:
//
// 2012-06-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - updated location; simulation of optical transmittance
//
// 2012-02-22 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more appropriate geometry parameterisation
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-10-30
// ---------------------------------------------------------------------

#ifndef CedarExternalLens_H
#define CedarExternalLens_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "CedarMaterialParameters.hh"
#include "globals.hh"
#include "G4OpticalSurface.hh"
#include "G4LogicalSkinSurface.hh"
#include "G4Cons.hh"
#include "CedarPMT.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarExternalLens : public NA62VComponent {

public:

  CedarExternalLens(G4Material*, G4LogicalVolume*, G4int);
  ~CedarExternalLens() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurface();
  void SetProperties();

  G4int         GetCopyNumber()                    { return iCopyNumber;     }
  void          SetCopyNumber(G4int value)         { iCopyNumber = value;    }
  G4String      GetName()                          { return fName;           }
  void          SetName(G4String value)            { fName = value;          }
  G4ThreeVector GetPosition()                      { return fPosition;       }
  void          SetPosition(G4ThreeVector value)   { fPosition = value;      }
  G4double      GetDiameter()                      { return fDiameter;       }
  void          SetDiameter(G4double value)        { fDiameter = value;      }
  G4double      GetSurfaceRadius()                 { return fSurfaceRadius;  }
  void          SetSurfaceRadius(G4double value)   { fSurfaceRadius = value; }
  G4double      GetZLength()                       { return fZLength;        }
  void          SetZLength(G4double value)         { fZLength = value;       }

private:

  G4int         iLGType;
  G4int         iCopyNumber;
  G4String      fName;
  G4ThreeVector fPosition;
  G4double      fDiameter;
  G4double      fMinThickness;
  G4double      fSurfaceRadius;
  G4double      fZLength;

  G4OpticalSurface     *fOpticalSurface;
  G4LogicalSkinSurface *fLogicalSkinSurface;
};

#endif
