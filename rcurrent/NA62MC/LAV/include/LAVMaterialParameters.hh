// LAVMaterialParameters.hh
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// 2009-03-02 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - First implementation of LAV materials
// 2011-01-24 Domenico Di Filippo (difilippo@na.infn.it)
//   - Added Optical surface
//
// --------------------------------------------------------------
#ifndef LAVMaterialParameters_H
#define LAVMaterialParameters_H 1

#include "globals.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"
#include "TObjArray.h"
#include "G4OpticalSurface.hh"

class LAVMaterialParameters
{

public:

  ~LAVMaterialParameters();
  static LAVMaterialParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static LAVMaterialParameters* fInstance;

protected:

  LAVMaterialParameters();
  void ReadPMsData(G4int);
  // Method to define materials used for detector
  void DefineMaterials();
  void DefineSurfaces();
  void SetMaterialProperties();

public:

  G4int                GetMaterialPropertiesNEntries()                    { return fMaterialPropertiesNEntries;   };
  void                 SetMaterialPropertiesNEntries(G4int value)         { fMaterialPropertiesNEntries = value;  };
  
  // Optical surfaces used in the LAVAccurateBlock
  G4OpticalSurface* GetDiffusiveSurface()     {return fDiffusiveSurface;} 
  G4OpticalSurface* GetBadReflectiveSurface() {return fBadReflactiveSurface;}
  G4OpticalSurface* GetPhotonKillerSurface()  {return fPhotonKillerSurface;}

private:

  G4int fMaterialPropertiesNEntries;
  G4OpticalSurface *fDiffusiveSurface, *fBadReflactiveSurface, *fPhotonKillerSurface;

};
#endif
