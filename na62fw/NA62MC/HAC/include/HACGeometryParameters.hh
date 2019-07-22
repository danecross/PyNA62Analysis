#ifndef HACGeometryParameters_H
#define HACGeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"
#include "G4ThreeVector.hh"

#include "NA62VGeometryParameters.hh"

class HACGeometryParameters : public NA62VGeometryParameters
{

public:

  ~HACGeometryParameters();
  static HACGeometryParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static HACGeometryParameters* fInstance;

protected:

  HACGeometryParameters();

public:

  G4double GetHACRespRegionZStart()    {return fHACRespRegionZStart;};
  G4double GetHACRespRegionZEnd()      {return fHACRespRegionZEnd;};
  G4double GetHACRespRegionZCenter()   {return fHACRespRegionZCenter;};
  G4double GetHACRespRegionXLength()   {return fHACRespRegionXLength;};
  G4double GetHACRespRegionYLength()   {return fHACRespRegionYLength;};
  G4double GetHACRespRegionZLength()   {return fHACRespRegionZLength;};

  G4double GetHACMagnetZPosition()     {return fHACMagnetZPosition;};
  G4double GetHACMagnetXLength()       {return fHACMagnetXLength;};
  G4double GetHACMagnetYLength()       {return fHACMagnetYLength;};
  G4double GetHACMagnetZLength()       {return fHACMagnetZLength;};
  G4double GetHACMagnetFieldStrength() {return fHACMagnetFieldStrength;};

  G4double GetAbsorberLayerXLength()   {return fAbsorberLayerXLength;};
  G4double GetAbsorberLayerYLength()   {return fAbsorberLayerYLength;};
  G4double GetAbsorberLayerZLength()   {return fAbsorberLayerZLength;};

  G4double GetScintillatorLayerXLength()   {return fScintillatorLayerXLength;};
  G4double GetScintillatorLayerYLength()   {return fScintillatorLayerYLength;};
  G4double GetScintillatorLayerZLength()   {return fScintillatorLayerZLength;};

  G4int    GetNLayers()                    {return fNLayers;};

  G4double GetHACModuleXLength()       {return fHACModuleXLength;};
  G4double GetHACModuleYLength()       {return fHACModuleYLength;};
  G4double GetHACModuleZLength()       {return fHACModuleZLength;};

  G4double GetHACDetectorZPosition()   {return fHACDetectorZPosition;};
  G4double GetHACDetectorZLength()     {return fHACDetectorZLength;};
  G4double GetHACDetectorYRotation()   {return fHACDetectorYRotation;};

  G4double GetHACModuleXPosition(G4int i) {return fHACModuleXPosition[i];};
  G4double GetHACModuleYPosition(G4int i) {return fHACModuleYPosition[i];};

private:

  G4double fHACRespRegionZStart;
  G4double fHACRespRegionZEnd;
  G4double fHACRespRegionZCenter;
  G4double fHACRespRegionXLength;
  G4double fHACRespRegionYLength;
  G4double fHACRespRegionZLength;

  G4double fHACMagnetZPosition;
  G4double fHACMagnetZLength;
  G4double fHACMagnetFieldStrength;
  G4double fHACMagnetXLength;
  G4double fHACMagnetYLength;

  G4double fAbsorberLayerXLength;
  G4double fAbsorberLayerYLength;
  G4double fAbsorberLayerZLength;

  G4double fScintillatorLayerXLength;
  G4double fScintillatorLayerYLength;
  G4double fScintillatorLayerZLength; 

  G4int fNLayers;

  G4double fHACModuleXLength;
  G4double fHACModuleYLength;
  G4double fHACModuleZLength;

  G4double fHACDetectorZPosition;
  G4double fHACDetectorZLength;
  G4double fHACDetectorYRotation;

  G4double fHACModuleXPosition[9];
  G4double fHACModuleYPosition[9];

};
#endif
