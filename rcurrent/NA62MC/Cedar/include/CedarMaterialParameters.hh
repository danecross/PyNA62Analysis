// --------------------------------------------------------------
// History:
//
// 2012-12-21 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - parameters are read from a configuration file
//
// 2012-10-26 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - the measured QW transmittances (different for each QW)
//
// 2012-08-16 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - improved transmittance parameterizations
//
// 2012-06-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - optical cap lens transmittance
//
// 2012-02-22 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more photodetector configurations added
//
// 2011-11-18 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - switching between West/North, various gas options
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - absorption, reflectivity, PMT quantum efficiency
//
// 2011-06-10 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - optical properties of H2, N2, quartz
// - optical surfaces
//
// 2009-11-16 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Cedar hydrogen parameters (P, T, density) added
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef CedarMaterialParameters_H
#define CedarMaterialParameters_H 1

#include "TObjArray.h"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"
#include "G4MaterialPropertiesTable.hh"

// Must be 0 for tracking and 1 for optimal visualisation
#define CEDAR_VIS 0

///////////////////////////////////////////////////////////////

class CedarMaterialParameters {

public:

  ~CedarMaterialParameters();
  static CedarMaterialParameters* GetInstance();
  void RemoveOpticalProperties();
  TObjArray GetHashTable();
  void Print();

  G4bool OpticalPropertiesEnabled() { return fOpticalPropertiesEnabled; }

private:

  void ParseConfFile(TString);
  static CedarMaterialParameters* fInstance;

protected:

  CedarMaterialParameters();
  void DefineMaterials();
  void DefineProperties();
  void BuildPropertyTables();
  void AssignOpticalProperties();

  G4double H2RefIndex(G4double, G4double, G4double);
  G4double H2RefIndex_Lau(G4double, G4double, G4double);
  G4double N2RefIndex(G4double, G4double, G4double);
  G4double NeRefIndex(G4double, G4double, G4double);
  G4double HeRefIndex(G4double, G4double, G4double);
  G4double QuartzRefIndex(G4double);
  G4double QuartzRefIndex_Lau(G4double);

  G4double ExternalLensTransmittance(G4double Wavelength);
  G4double QuartzWindowTransmittance(G4int iWindow, G4double Wavelength);
  G4double QuartzWindowAndFilterTransmittance(G4double Wavelength);

  G4double QuartzAbsorptionLength(G4double Wavelength);
  G4double ExternalLensAbsorptionLength(G4double Wavelength);
  G4double QuartzWindowAbsorptionLength(G4int iWindow, G4double Wavelength);
  G4double QuartzWindowAndFilterAbsorptionLength(G4double Wavelength);

  G4double ManginMirrorReflectivity(G4double Wavelength);
  G4double SphericalMirrorAndConeReflectivity(G4double Wavelength);

  G4double ELConvert(G4double);

public:

  G4int     GetMaterialPropertiesNEntries()              { return fMaterialPropertiesNEntries; }
  void      SetMaterialPropertiesNEntries(G4int val)     { fMaterialPropertiesNEntries = val;  }

  // Radiator gases

  G4double  GetRadiatorHydrogenTemperature()             { return fRadiatorHydrogenTemperature;}
  void      SetRadiatorHydrogenTemperature(G4double val) { fRadiatorHydrogenTemperature = val; }
  G4double  GetRadiatorHydrogenPressure()                { return fRadiatorHydrogenPressure;   }
  void      SetRadiatorHydrogenPressure(G4double val)    { fRadiatorHydrogenPressure = val;    }
  G4double  GetRadiatorHydrogenDensity()                 { return fRadiatorHydrogenDensity;    }
  void      SetRadiatorHydrogenDensity(G4double val)     { fRadiatorHydrogenDensity = val;     }

  G4double  GetRadiatorNitrogenTemperature()             { return fRadiatorNitrogenTemperature;}
  void      SetRadiatorNitrogenTemperature(G4double val) { fRadiatorNitrogenTemperature = val; }
  G4double  GetRadiatorNitrogenPressure()                { return fRadiatorNitrogenPressure;   }
  void      SetRadiatorNitrogenPressure(G4double val)    { fRadiatorNitrogenPressure = val;    }
  G4double  GetRadiatorNitrogenDensity()                 { return fRadiatorNitrogenDensity;    }
  void      SetRadiatorNitrogenDensity(G4double val)     { fRadiatorNitrogenDensity = val;     }

  // The nitrogen enclosure

  G4double  GetNitrogenPressure()                        { return fNitrogenPressure;           }
  void      SetNitrogenPressure(G4double val)            { fNitrogenPressure = val;            }
  G4double  GetNitrogenTemperature()                     { return fNitrogenTemperature;        }
  void      SetNitrogenTemperature(G4double val)         { fNitrogenTemperature = val;         }
  G4double  GetNitrogenDensity()                         { return fNitrogenDensity;            }
  void      SetNitrogenDensity(G4double val)             { fNitrogenDensity = val;             }

  // Optical surfaces

  G4MaterialPropertiesTable *GetManginMirrorOpticalSurfacePT()
  { return fManginMirrorOpticalSurfacePT; }
  G4MaterialPropertiesTable *GetSphericalMirrorOpticalSurfacePT()
  { return fSphericalMirrorOpticalSurfacePT; }
  G4MaterialPropertiesTable *GetLensOpticalSurfacePT()
  { return fLensOpticalSurfacePT; }
  G4MaterialPropertiesTable *GetLightGuideOpticalSurfacePT()
  { return fLightGuideOpticalSurfacePT; }

private:

  G4bool    fOpticalPropertiesEnabled;

  G4double  fRadiatorHydrogenPressure;
  G4double  fRadiatorHydrogenTemperature;
  G4double  fRadiatorHydrogenDensity;

  G4double  fRadiatorNitrogenPressure;
  G4double  fRadiatorNitrogenTemperature;
  G4double  fRadiatorNitrogenDensity;

  G4double  fNitrogenPressure;
  G4double  fNitrogenTemperature;
  G4double  fNitrogenDensity;

  G4int     fMaterialPropertiesNEntries;
  G4double  fCherenkovLambdaMin;
  G4double  fCherenkovLambdaMax;
  G4double  fCherenkovPhotonEnergyMin;
  G4double  fCherenkovPhotonEnergyMax;

  G4double* fPhotonEnergy;
  G4double* fWaveLength;

  G4double* fRadiatorHydrogenRefIndex;
  G4double* fRadiatorNitrogenRefIndex;

  G4double* fNitrogenRefIndex;
  G4double* fQuartzRefIndex;
  G4double* fQuartzAbsorptionLength;
  G4double* fExternalLensQuartzAbsorptionLength;
  G4double* fQuartzWindowQuartzAbsorptionLength; // contains 8 tables for 8 sectors 

  G4double* fManginMirrorReflectivity;
  G4double* fManginMirrorEfficiency;
  G4double* fManginMirrorSpecularLobe;
  G4double* fManginMirrorSpecularSpike;

  G4double* fSphericalMirrorReflectivity;
  G4double* fSphericalMirrorEfficiency;
  G4double* fSphericalMirrorSpecularLobe;
  G4double* fSphericalMirrorSpecularSpike;

  G4double* fLightGuideReflectivity;
  G4double* fLightGuideEfficiency;
  G4double* fLightGuideSpecularLobe;
  G4double* fLightGuideSpecularSpike;

  G4double* fLensTransmittance;

  G4MaterialPropertiesTable* fRadiatorHydrogenMPT;
  G4MaterialPropertiesTable* fRadiatorNitrogenMPT;

  G4MaterialPropertiesTable* fNitrogenMPT;
  G4MaterialPropertiesTable* fQuartzMPT;
  G4MaterialPropertiesTable* fIdealQuartzMPT;
  G4MaterialPropertiesTable* fExternalLensQuartzMPT;
  G4MaterialPropertiesTable* fQuartzWindowQuartzMPT[8];

  G4MaterialPropertiesTable* fManginMirrorOpticalSurfacePT;
  G4MaterialPropertiesTable* fSphericalMirrorOpticalSurfacePT;
  G4MaterialPropertiesTable* fLensOpticalSurfacePT;
  G4MaterialPropertiesTable* fLightGuideOpticalSurfacePT;
};
#endif
