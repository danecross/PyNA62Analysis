// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-17
// ---------------------------------------------------------------------

#ifndef CedarDiaphragm_H
#define CedarDiaphragm_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "CedarMaterialParameters.hh"
#include "globals.hh"

#include "G4OpticalSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarDiaphragm : public NA62VComponent {

public:
  
  CedarDiaphragm(G4Material*, G4LogicalVolume*);
  ~CedarDiaphragm() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties() {}

public:

  G4ThreeVector GetPosition()                    { return fPosition;       }
  void          SetPosition(G4ThreeVector value) { fPosition = value;      }
  G4double      GetZLength()                     { return fZLength;        }
  void          SetZLength(G4double value)       { fZLength = value;       }
  G4double      GetInnerRadius()                 { return fInnerRadius;    }
  void          SetInnerRadius(G4double value)   { fInnerRadius = value;   }
  G4double      GetOuterRadius()                 { return fOuterRadius;    }
  void          SetOuterRadius(G4double value)   { fOuterRadius = value;   }
  G4double      GetOpeningRadius()               { return fOpeningRadius;  }
  void          SetOpeningRadius(G4double value) { fOpeningRadius = value; }
  G4double      GetApertureR()                   { return fApertureR;      }
  void          SetApertureR(G4double value)     { fApertureR = value;     }
  G4double      GetAperturePhi()                 { return fAperturePhi;    }
  void          SetAperturePhi(G4double value)   { fAperturePhi = value;   }

private:

  G4int         iGasH2;
  G4ThreeVector fPosition;
  G4double      fZLength;
  G4double      fInnerRadius;
  G4double      fOuterRadius;
  G4double      fOpeningRadius;
  G4double      fAperture;
  G4double      fApertureR;
  G4double      fAperturePhi;

  G4Tubs          **fSolidGap;
  G4LogicalVolume **fLogicalGap;
};

#endif
