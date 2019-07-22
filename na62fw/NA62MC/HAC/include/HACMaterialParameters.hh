#ifndef HACMaterialParameters_H
#define HACMaterialParameters_H 1

#include "globals.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"
#include "TObjArray.h"

class HACMaterialParameters
{

public:

  ~HACMaterialParameters();
  static HACMaterialParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static HACMaterialParameters* fInstance;

protected:
  HACMaterialParameters();
  void ReadPMsData(G4int);

  // Method to define materials used for detector
  void DefineMaterials();
  void SetMaterialProperties();

public:
  G4int                GetMaterialPropertiesNEntries()                    { return fMaterialPropertiesNEntries;   };
  void                 SetMaterialPropertiesNEntries(G4int value)         { fMaterialPropertiesNEntries = value;  };

private:

  G4int fMaterialPropertiesNEntries;

};
#endif
