// ---------------------------------------------------------------------
// History:
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - cones and PMTs added
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-16
// ---------------------------------------------------------------------

#ifndef CedarLightGuide2011_H
#define CedarLightGuide2011_H 1

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

class CedarLightGuide2011 : public NA62VComponent {

public:

  CedarLightGuide2011(G4Material*, G4LogicalVolume*, G4int);
  ~CedarLightGuide2011() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurface();
  void SetProperties() {}

  G4int         GetCopyNumber()                   { return iCopyNumber;   }
  void          SetCopyNumber(G4int value)        { iCopyNumber = value;  }
  G4String      GetName()                         { return fName;         }
  void          SetName(G4String value)           { fName = value;        }
  G4ThreeVector GetPosition()                     { return fPosition;     }
  void          SetPosition(G4ThreeVector value)  { fPosition = value;    }
  G4double      GetDiameter()                     { return fDiameter;     }
  void          SetDiameter(G4double value)       { fDiameter = value;    }
  G4double      GetLength()                       { return fLength;       }
  void          SetLength(G4double value)         { fLength = value;      }

private:

  G4int         iCopyNumber;
  G4String      fName;
  G4ThreeVector fPosition;
  G4double      fDiameter;
  G4double      fLength;

  G4int         fNConesTotal;
  G4int         fNofRows;
  G4int*        fNofCones;
  G4double*     fRowsPhiShift;
  G4double      fConesPhiShift;
  G4double      fConesThetaShift;

  G4double      fConeInnerRadius;
  G4double      fConeOuterRadius;
  G4double      fConeLength;
  G4double      fPMTLength;
  G4bool*       fConePMTExists;

  CedarPMT**          fPMT;
  G4VPhysicalVolume** fConePhysicalVolume;
  G4OpticalSurface*   fOpticalSurface;
};

#endif
