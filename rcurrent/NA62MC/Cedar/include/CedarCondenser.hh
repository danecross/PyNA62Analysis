// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-16
// ---------------------------------------------------------------------

#ifndef CedarCondenser_H
#define CedarCondenser_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "G4OpticalSurface.hh"
#include "G4LogicalSkinSurface.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarCondenser : public NA62VComponent {

public:
  
  CedarCondenser(G4Material*, G4LogicalVolume*, G4int);
  ~CedarCondenser() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurface();
  void SetProperties();

public:

  G4int         GetCopyNumber()                      { return fCopyNumber;          }
  void          SetCopyNumber(G4int val)             { fCopyNumber = val;           }
  G4String      GetName()                            { return fName;                }
  void          SetName(G4String val)                { fName = val;                 }
  G4ThreeVector GetPosition()                        { return fPosition;            }
  void          SetPosition(G4ThreeVector val)       { fPosition = val;             }
  G4double      GetZLength()                         { return fZLength;             }
  void          SetZLength(G4double val)             { fZLength = val;              }
  G4double      GetOuterRadius()                     { return fOuterRadius;         }
  void          SetOuterRadius(G4double val)         { fOuterRadius = val;          }
  G4double      GetRadialOffset()                    { return fRadialOffset;        }
  void          SetRadialOffset(G4double val)        { fRadialOffset = val;         }
  G4double      GetDistanceToCentre()                { return fDistanceToCentre;    }
  void          SetDistanceToCentre(G4double val)    { fDistanceToCentre = val;     }
  G4double      GetFrontSurfaceRadius()              { return fFrontSurfaceRadius;  }
  void          SetFrontSurfaceRadius(G4double val)  { fFrontSurfaceRadius = val;   }
  G4double      GetInterCondenserAngle()             { return fInterCondenserAngle; }
  void          SetInterCondenserAngle(G4double val) { fInterCondenserAngle = val;  }

  G4OpticalSurface*     GetOpticalSurface()          { return fOpticalSurface;      }
  G4LogicalSkinSurface* GetLogicaSkinSurface()       { return fLogicalSkinSurface;  }

private:

  G4int         fCopyNumber;
  G4String      fName;
  G4double      fAngle;
  G4ThreeVector fPosition;
  G4double      fZStart;
  G4double      fZLength;
  G4double      fOuterRadius;
  G4double      fRadialOffset;
  G4double      fDistanceToCentre;
  G4double      fFrontSurfaceRadius;
  G4double      fInterCondenserAngle;

  G4RotationMatrix     *fRotationMatrix;
  G4OpticalSurface     *fOpticalSurface;
  G4LogicalSkinSurface *fLogicalSkinSurface;
};

#endif
