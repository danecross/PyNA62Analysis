#ifndef MUV0MaterialParameters_H
#define MUV0MaterialParameters_H 1

#include "globals.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"
#include "TObjArray.h"

class MUV0MaterialParameters {

public:

  ~MUV0MaterialParameters() {}
  static MUV0MaterialParameters* GetInstance();
  TObjArray GetHashTable();
  void Print() {}

private:

  static MUV0MaterialParameters* fInstance;

protected:
  MUV0MaterialParameters();
  void ReadPMsData(G4int);

  // Method to define materials used for detector
  void DefineMaterials();
  void SetMaterialProperties();

public:
  G4int GetMaterialPropertiesNEntries()            { return fMaterialPropertiesNEntries;  }
  void  SetMaterialPropertiesNEntries(G4int value) { fMaterialPropertiesNEntries = value; }

private:

  G4int fMaterialPropertiesNEntries;

};
#endif
