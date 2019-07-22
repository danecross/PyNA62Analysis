// ---------------------------------------------------------------------
// History:
//
// 2012-06-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Atex cylinder added, minor updates
//
// 2012-02-22 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more photodetector configurations added
//
// 2011-11-18 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - external cap lenses added
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-13
// ---------------------------------------------------------------------

#ifndef CedarOctant_H
#define CedarOctant_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "globals.hh"

#include "CedarAtexCylinder.hh"
#include "CedarSphericalMirror.hh"
#include "CedarSphericalMirrorMount.hh"
#include "CedarLightGuide.hh"
#include "CedarLightGuide2011.hh"
#include "CedarExternalLens.hh"
#include "CedarOldPMT.hh"
#include "G4OpticalSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarOctant : public NA62VComponent {

public:

  CedarOctant(G4Material*, G4LogicalVolume*, G4int);
  ~CedarOctant() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

  G4int         GetCopyNumber()                  { return fCopyNumber;    }
  void          SetCopyNumber(G4int value)       { fCopyNumber = value;   }
  G4String      GetName()                        { return fName;          }
  void          SetName(G4String value)          { fName = value;         }
  G4ThreeVector GetPosition()                    { return fPosition;      }
  void          SetPosition(G4ThreeVector value) { fPosition = value;     }
  G4double      GetMinRadius()                   { return fMinRadius;     }
  void          SetMinRadius(G4double value)     { fMinRadius = value;    }
  G4double      GetMaxRadius()                   { return fMaxRadius;     }
  void          SetMaxRadius(G4double value)     { fMaxRadius = value;    }
  G4double      GetCentralAngle()                { return fCentralAngle;  }
  void          SetCentralAngle(G4double value)  { fCentralAngle = value; }
  G4double      GetZLength()                     { return fZLength;       }
  void          SetZLength(G4double value)       { fZLength = value;      }

private:

  G4int         iLGType;
  G4int         iEnabled;
  G4int         fCopyNumber;
  G4String      fName;
  G4ThreeVector fPosition;
  G4double      fMinRadius;
  G4double      fMaxRadius;
  G4double      fCentralAngle;
  G4double      fZLength;

  CedarAtexCylinder*         fAtexCylinder;
  CedarSphericalMirror*      fMirror;
  CedarSphericalMirrorMount* fMirrorMount;
  CedarLightGuide*           fLightGuide;
  CedarLightGuide2011*       fLightGuide2011;
  CedarExternalLens*         fExternalLens;
  CedarOldPMT*               fOldPMT;
};

#endif
