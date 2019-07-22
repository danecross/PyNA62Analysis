// ---------------------------------------------------------------------
// History:
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - cones and PMTs added
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-16
// ---------------------------------------------------------------------

#ifndef CedarLightGuide_H
#define CedarLightGuide_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "CedarMaterialParameters.hh"
#include "G4OpticalSurface.hh"
#include "G4LogicalSkinSurface.hh"
#include "G4Cons.hh"
#include "CedarPMT.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarLightGuide : public NA62VComponent {

public:

  CedarLightGuide(G4Material*, G4LogicalVolume*, G4int);
  ~CedarLightGuide() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurface();
  void SetProperties() {}

  G4int         GetCopyNumber()                { return iCopyNumber;               }
  void          SetCopyNumber(G4int val)       { iCopyNumber = val;                }
  G4String      GetName()                      { return fName;                     }
  void          SetName(G4String val)          { fName = val;                      }
  G4ThreeVector GetPosition()                  { return fPosition;                 }
  void          SetPosition(G4ThreeVector val) { fPosition = val;                  }
  G4double      GetInnerRadius()               { return fInnerRadius;              }
  void          SetInnerRadius(G4double val)   { fInnerRadius = val;               }
  G4double      GetOuterRadius()               { return fOuterRadius;              }
  void          SetOuterRadius(G4double val)   { fOuterRadius = val;               }
  G4double      GetDepth()                     { return fOuterRadius-fInnerRadius; }
  G4double      GetDiameter()                  { return fDiameter;                 }
  void          SetDiameter(G4double val)      { fDiameter = val;                  }

private:

  G4int         iLGType;
  G4int         iCopyNumber;
  G4String      fName;
  G4ThreeVector fPosition;
  G4double      fInnerRadius;
  G4double      fOuterRadius;
  G4double      fDiameter;

  G4int         fNConesTotal;
  G4int         fNofRows;
  G4int*        fNofCones;
  G4int*        fInstrumented;
  G4double*     fRowsPhiShift;
  G4double      fConesPhiShift;
  G4double      fConesThetaShift;

  G4double      fConeInnerRadius;
  G4double      fConeOuterRadius;
  G4double      fConeLength;
  G4double      fPMTLength;

  CedarPMT**          fPMT;
  G4VPhysicalVolume** fConePhysicalVolume;
  G4OpticalSurface*   fOpticalSurface;
};

#endif
