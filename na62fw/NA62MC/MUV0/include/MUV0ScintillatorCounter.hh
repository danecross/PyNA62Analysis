#ifndef MUV0ScintillatorCounter_H
#define MUV0ScintillatorCounter_H 1

#include "NA62VComponent.hh"
#include "MUV0GeometryParameters.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class MUV0ScintillatorCounter : public NA62VComponent {

public:

  MUV0ScintillatorCounter(G4Material*, G4LogicalVolume*, G4ThreeVector, G4ThreeVector, G4int);
  ~MUV0ScintillatorCounter() {}
  void ReadGeometryParameters() {}
  void CreateGeometry();
  void SetProperties();

public:

  G4int GetID()                                { return fID;       }
  void  SetID(G4int val)                       { fID = val;        }
  G4ThreeVector GetSize()                      { return fSize;     }
  void          SetSize(G4ThreeVector val)     { fSize = val;      }
  G4ThreeVector GetPosition()                  { return fPosition; }
  void          SetPotision(G4ThreeVector val) { fPosition = val;  }

private:

  G4int         fID;
  G4String      fName;
  G4ThreeVector fSize;
  G4ThreeVector fPosition;

};

#endif
