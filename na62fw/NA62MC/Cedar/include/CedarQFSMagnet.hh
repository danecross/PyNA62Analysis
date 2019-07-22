// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (Evgueni.Goudzovski@cern.ch) 2009-11-16
// ---------------------------------------------------------------------

#ifndef CedarQFSMagnet_H
#define CedarQFSMagnet_H 1

#include "NA62VComponent.hh"
#include "CedarGeometryParameters.hh"
#include "G4Polyhedra.hh"
#include "globals.hh"

#include "G4FieldManager.hh"
#include "G4QuadrupoleMagField.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CedarQFSMagnet : public NA62VComponent {

public:
  
  CedarQFSMagnet(G4Material*, G4LogicalVolume*, G4ThreeVector, G4double, G4int);
  ~CedarQFSMagnet() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties() {}

public:

  G4ThreeVector GetPosition()                    { return fPosition;    }
  void          SetPosition(G4ThreeVector val)   { fPosition = val;     }
  G4int         GetiCopy()                       { return fiCopy;       }
  void          SetiCopy(G4int val)              { fiCopy = val;        }
  G4String      GetName()                        { return fName;        }
  void          SetName(G4String val)            { fName = val;         }
  G4String      GetNameBH()                      { return fNameBH;      }
  void          SetNameBH(G4String val)          { fNameBH = val;       }
  G4String      GetNameYoke()                    { return fNameYoke;    }
  void          SetNameYoke(G4String val)        { fNameYoke = val;     }
  G4double      GetInnerRadius()                 { return fInnerRadius; }
  void          SetInnerRadius(G4double val)     { fInnerRadius = val;  }
  G4double      GetOuterRadius()                 { return fOuterRadius; }
  void          SetOuterRadius(G4double val)     { fOuterRadius = val;  }
  G4double      GetZLength()                     { return fZLength;     }
  void          SetZLength(G4double val)         { fZLength = val;      }
  G4double      GetGradient()                    { return fGradient;    }
  void          SetGradient(G4double val)        { fGradient = val;     }

  G4FieldManager*       GetFieldMgr()                          { return fFieldMgr; }
  void                  SetFieldMgr(G4FieldManager* val)       { fFieldMgr = val;  }
  G4QuadrupoleMagField* GetMagField()                          { return fMagField; }
  void                  SetMagField(G4QuadrupoleMagField* val) { fMagField = val;  }

private:

  G4ThreeVector fPosition;
  G4int         fiCopy;
  G4String      fName, fNameBH, fNameYoke;
  G4double      fInnerRadius, fOuterRadius;
  G4double      fZLength;
  G4double      fGradient; ///< Run-dependent field gradient (dBx/dy and dBy/dx)

  G4VPhysicalVolume*    fPhysiMagnet;
  G4Tubs*               fSolidBlackHole;
  G4LogicalVolume*      fLogicBlackHole;
  G4FieldManager*       fFieldMgr;
  G4QuadrupoleMagField* fMagField;
};

#endif
