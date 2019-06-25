// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-05-16
// ---------------------------------------------------------------------

#ifndef CedarManginMirror_H
#define CedarManginMirror_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "globals.hh"

#include "G4OpticalSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarManginMirror : public NA62VComponent {

public:
  
  CedarManginMirror(G4Material*, G4LogicalVolume*);
  ~CedarManginMirror() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void DefineOpticalSurface();
  void SetProperties();

public:

  G4ThreeVector GetPosition()                   { return fPosition;       }
  void          SetPosition(G4ThreeVector val)  { fPosition = val;        }
  G4double      GetZLength()                    { return fZLength;        }
  void          SetZLength(G4double val)        { fZLength = val;         }
  G4double      GetCoatingZLength()             { return fCoatingZLength; }
  void          SetCoatingZLength(G4double val) { fCoatingZLength = val;  }
  G4double      GetInnerRadius()                { return fInnerRadius;    }
  void          SetInnerRadius(G4double val)    { fInnerRadius = val;     }
  G4double      GetOuterRadius()                { return fOuterRadius;    }
  void          SetOuterRadius(G4double val)    { fOuterRadius = val;     }

  G4double GetReflectingSurfaceRadius()           { return fReflectingSurfaceRadius;  }
  void SetReflectingSurfaceRadius(G4double val)   { fReflectingSurfaceRadius = val;   }
  G4double GetRefractingSurfaceRadius()           { return fRefractingSurfaceRadius;  }
  void SetRefractingSurfaceRadius(G4double val)   { fRefractingSurfaceRadius = val;   }

  G4VPhysicalVolume *GetLensPhysicalVolume()      { return fLensPhysicalVolume;       }
  G4VPhysicalVolume *GetCoatingPhysicalVolume()   { return fCoatingPhysicalVolume;    }

  G4OpticalSurface* GetReflectingOpticalSurface() { return fReflectingOpticalSurface; }
  G4OpticalSurface* GetRefractingOpticalSurface() { return fRefractingOpticalSurface; }

private:

  G4ThreeVector fPosition;
  G4double      fZLength;
  G4double      fCoatingZLength;
  G4double      fInnerRadius;
  G4double      fOuterRadius;
  G4double      fReflectingSurfaceRadius;
  G4double      fRefractingSurfaceRadius;

  G4VPhysicalVolume *fLensPhysicalVolume;
  G4VPhysicalVolume *fCoatingPhysicalVolume;

  G4OpticalSurface* fReflectingOpticalSurface;
  G4OpticalSurface* fRefractingOpticalSurface;
};

#endif
