// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-16
// ---------------------------------------------------------------------

#ifndef CedarChromaticCorrector_H
#define CedarChromaticCorrector_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "globals.hh"
#include "G4OpticalSurface.hh"
#include "G4LogicalSkinSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarChromaticCorrector : public NA62VComponent {

public:
  
  CedarChromaticCorrector(G4Material*, G4LogicalVolume*);
  ~CedarChromaticCorrector() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurface();
  void SetProperties();

public:

  G4String      GetName()                        { return fName;               }
  void          SetName(G4String value)          { fName = value;              }
  G4ThreeVector GetPosition()                    { return fPosition;           }
  void          SetPosition(G4ThreeVector value) { fPosition = value;          }
  G4double      GetZLength()                     { return fZLength;            }
  void          SetZLength(G4double value)       { fZLength = value;           }
  G4double      GetInnerRadius()                 { return fInnerRadius;        }
  void          SetInnerRadius(G4double value)   { fInnerRadius = value;       }
  G4double      GetOuterRadius()                 { return fOuterRadius;        }
  void          SetOuterRadius(G4double value)   { fOuterRadius = value;       }

  G4double GetRearSurfaceRadius()                { return fRearSurfaceRadius;  }
  void     SetRearSurfaceRadius(G4double value)  { fRearSurfaceRadius = value; }

  G4OpticalSurface* GetOpticalSurface()          { return fOpticalSurface;     }
  G4LogicalSkinSurface* GetLogicaSkinSurface()   { return fLogicalSkinSurface; }

private:

  G4String      fName;
  G4ThreeVector fPosition;
  G4double      fZLength;
  G4double      fInnerRadius;
  G4double      fOuterRadius;
  G4double      fRearSurfaceRadius;

  G4OpticalSurface* fOpticalSurface;
  G4LogicalSkinSurface *fLogicalSkinSurface;
};

#endif
