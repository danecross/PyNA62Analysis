#ifndef RICHMaterialParameters_H
#define RICHMaterialParameters_H 1

#include "globals.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"
#include "TObjArray.h"
#include "G4Material.hh"

class RICHMaterialParameters
{

public:

  ~RICHMaterialParameters();
  static RICHMaterialParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static RICHMaterialParameters* fInstance;

protected:

  RICHMaterialParameters();
  void ReadPMsData(G4int);
  // Method to define materials used for detector
  void DefineMaterials();
  void SetMaterialProperties();

public:

  G4double GetMeanNeonRefIndex()             {return fMeanNeonRefIndex; } // for fast sim
  void SetMeanNeonRefIndex(G4double value)   {fMeanNeonRefIndex = value; } // for fast sim

  G4int                GetMaterialPropertiesNEntries()                    { return fMaterialPropertiesNEntries;   };
  void                 SetMaterialPropertiesNEntries(G4int value)         { fMaterialPropertiesNEntries = value;  };
  G4double             GetMinWavelength()                                 { return fMinWavelength;                };
  void                 SetMinWavelength(G4double value)                   { fMinWavelength = value;               };

  G4double *           GetPhotonEnergy()                                  { return fPhotonEnergy;                 };
  void                 SetPhotonEnergy(G4double * value)                  { fPhotonEnergy = value;                };
  G4double *           GetNeonRefIndex()                                  { return fNeonRefIndex;                 };
  void                 SetNeonRefIndex(G4double * value)                  { fNeonRefIndex = value;                };
  G4double *           GetNeonScintilFast()                               { return fNeonScintilFast;              };
  void                 SetNeonScintilFast(G4double * value)               { fNeonScintilFast = value;             };
  G4double *           GetNeonAbsorptionLength()                          { return fNeonAbsorptionLength;         };
  void                 SetNeonAbsorptionLength(G4double * value)          { fNeonAbsorptionLength = value;        };
  G4double             GetNeonPressure()                                  { return fNeonPressure;                 };
  void                 SetNeonPressure(G4double value)                    { fNeonPressure = value;                };
  G4double             GetNeonTemperature()                               { return fNeonTemperature;              };
  void                 SetNeonTemperature(G4double value)                 { fNeonTemperature = value;             };

  G4double *           GetQuartzRefIndex()                                { return fQuartzRefIndex;               };
  void                 SetQuartzRefIndex(G4double * value)                { fQuartzRefIndex = value;              };
  G4double *           GetQuartzAbsorptionLength()                        { return fQuartzAbsorptionLength;       };
  void                 SetQuartzAbsorptionLength(G4double * value)        { fQuartzAbsorptionLength = value;      };

  G4double *           GetVacuumRefIndex()                                { return fVacuumRefIndex;               };
  void                 SetVacuumRefIndex(G4double * value)                { fVacuumRefIndex = value;              };

  G4Material *	       GetMirrorWindowInnerFlangeMaterial()       {return fMirrorWindowInnerFlangeMaterial;  };
  void                 SetMirrorWindowInnerFlangeMaterial(G4Material * value) {fMirrorWindowInnerFlangeMaterial = value; };
  G4Material *         GetMirrorWindowOuterFlangeMaterial()       {return fMirrorWindowOuterFlangeMaterial;  };
  void                 SetMirrorWindowOuterFlangeMaterial(G4Material * value) {fMirrorWindowOuterFlangeMaterial = value; };
  G4Material *         GetInterfaceRingMaterial()       {return fInterfaceRingMaterial;  };
  void                 SetInterfaceRingMaterial(G4Material * value) {fInterfaceRingMaterial = value; };


  G4double             GetMirrorOpticalSurfaceSigmaAlpha()                { return fMirrorOpticalSurfaceSigmaAlpha;};
  void                 SetMirrorOpticalSurfaceSigmaAlpha(G4double value)  { fMirrorOpticalSurfaceSigmaAlpha = value;};
  G4double *           GetMirrorReflectivity()                            { return fMirrorReflectivity;           };
  void                 SetMirrorReflectivity(G4double * value)            { fMirrorReflectivity = value;          };
  G4double *           GetMirrorEfficiency()                              { return fMirrorEfficiency;             };
  void                 SetMirrorEfficiency(G4double * value)              { fMirrorEfficiency = value;            };

  G4double *           GetConeReflectivity()                              { return fConeReflectivity;             };
  void                 SetConeReflectivity(G4double * value)              { fConeReflectivity = value;            };
  G4double *           GetConeEfficiency()                                { return fConeEfficiency;               };
  void                 SetConeEfficiency(G4double * value)                { fConeEfficiency = value;              };

  G4double             GetPMTsWindowOpticalSurfaceSigmaAlpha()             { return fPMTsWindowOpticalSurfaceSigmaAlpha; };
  void                 SetPMTsWindowOpticalSurfaceSigmaAlpha(G4double value)
                                                                          { fPMTsWindowOpticalSurfaceSigmaAlpha = value; };
  G4double *           GetPMTsWindowReflectivity()                         { return fPMTsWindowReflectivity;        };
  void                 SetPMTsWindowReflectivity(G4double * value)         { fPMTsWindowReflectivity = value;       };
  G4double *           GetPMTsWindowEfficiency()                           { return fPMTsWindowEfficiency;          };
  void                 SetPMTsWindowEfficiency(G4double * value)           { fPMTsWindowEfficiency = value;         };
  G4double *           GetPMTsWindowSpecularSpike()                        { return fPMTsWindowSpecularSpike;       };
  void                 SetPMTsWindowSpecularSpike(G4double * value)        { fPMTsWindowSpecularSpike = value;      };
  G4double *           GetPMTsWindowSpecularLobe()                         { return fPMTsWindowSpecularLobe;        };
  void                 SetPMTsWindowSpecularLobe(G4double * value)         { fPMTsWindowSpecularLobe = value;       };
  G4double *           GetPMTsWindowBackscatter()                          { return fPMTsWindowBackscatter;         };
  void                 SetPMTsWindowBackscatter(G4double * value)          { fPMTsWindowBackscatter = value;        };

  G4double             GetVesselOpticalSurfaceSigmaAlpha()                  { return fVesselOpticalSurfaceSigmaAlpha; };
  void                 SetVesselOpticalSurfaceSigmaAlpha(G4double value)    { fVesselOpticalSurfaceSigmaAlpha = value; };
  G4double *           GetVesselReflectivity()                              { return fVesselReflectivity;             };
  void                 SetVesselReflectivity(G4double * value)              { fVesselReflectivity = value;            };
  G4double *           GetVesselEfficiency()                                { return fVesselEfficiency;               };
  void                 SetVesselEfficiency(G4double * value)                { fVesselEfficiency = value;              };
  G4double *           GetVesselSpecularSpike()                             { return fVesselSpecularSpike;            };
  void                 SetVesselSpecularSpike(G4double * value)             { fVesselSpecularSpike = value;           };
  G4double *           GetVesselSpecularLobe()                              { return fVesselSpecularLobe;             };
  void                 SetVesselSpecularLobe(G4double * value)              { fVesselSpecularLobe = value;            };
  G4double *           GetVesselBackscatter()                               { return fVesselBackscatter;              };
  void                 SetVesselBackscatter(G4double * value)               { fVesselBackscatter = value;             };

  G4String             GetPMType(G4int iPM)                               { return fPMType[iPM];                  };
  void                 SetPMType(G4int iPM, G4String value)               { fPMType[iPM] = value;                 };
  G4double             GetPMPeakQE(G4int iPM)                             { return fPMPeakQE[iPM];                };
  void                 SetPMPeakQE(G4int iPM, G4double value)             { fPMPeakQE[iPM] = value;               };
  G4double *           GetPMQuantumEfficiency(G4int iPM)                  { return fPMQuantumEfficiency[iPM];     };
  void                 SetPMQuantumEfficiency(G4int iPM, G4double * value){ fPMQuantumEfficiency[iPM] = value;    };

  G4MaterialPropertiesTable *
                       GetNeonMPT()                                       { return fNeonMPT;                      };
  void                 SetNeonMPT(G4MaterialPropertiesTable * value)      { fNeonMPT = value;                     };
  G4MaterialPropertiesTable *
                       GetQuartzMPT()                                     { return fQuartzMPT;                    };
  void                 SetQuartzMPT(G4MaterialPropertiesTable * value)    { fQuartzMPT = value;                   };
  G4MaterialPropertiesTable *
                       GetVacuumMPT()                                     { return fVacuumMPT;                    };
  void                 SetVacuumMPT(G4MaterialPropertiesTable * value)    { fVacuumMPT = value;                   };

  G4MaterialPropertiesTable *
                       GetMirrorOpticalSurfacePT()                        { return fMirrorOpticalSurfacePT;       };
  void                 SetMirrorOpticalSurfacePT(G4MaterialPropertiesTable * value)
                                                                          { fMirrorOpticalSurfacePT = value;      };
  G4MaterialPropertiesTable *
                       GetConeOpticalSurfacePT()                          { return fConeOpticalSurfacePT;         };
  void                 SetConeOpticalSurfacePT(G4MaterialPropertiesTable * value)
                                                                          { fConeOpticalSurfacePT = value;        };
  G4MaterialPropertiesTable *
                       GetPMTsWindowOpticalSurfacePT()                     { return fPMTsWindowOpticalSurfacePT;    };
  void                 SetPMTsWindowOpticalSurfacePT(G4MaterialPropertiesTable * value)
                                                                          { fPMTsWindowOpticalSurfacePT = value;   };
  G4MaterialPropertiesTable *
                       GetVesselOpticalSurfacePT()                          { return fVesselOpticalSurfacePT;         };
  void                 SetVesselOpticalSurfacePT(G4MaterialPropertiesTable * value)
                                                                          { fVesselOpticalSurfacePT = value;        };

  G4MaterialPropertiesTable *
                       GetPhotocatodePT(G4int iPM)                        { return fPhotocatodePT[iPM];           };
  void                 SetPhotocatodePT(G4MaterialPropertiesTable ** value)
                                                                          { fPhotocatodePT = value;               };

private:

  G4double fMeanNeonRefIndex;       // for fast sim

  G4int fMaterialPropertiesNEntries;
  G4double fMinWavelength;

  G4double* fPhotonEnergy;
  G4double* fNeonRefIndex;
  G4double* fNeonScintilFast;
  G4double* fNeonAbsorptionLength;
  G4double  fNeonPressure;
  G4double  fNeonTemperature;
	  
  G4double* fQuartzRefIndex;
  G4double* fQuartzAbsorptionLength;

  G4double* fVacuumRefIndex;

  G4Material* fMirrorWindowInnerFlangeMaterial;	
  G4Material* fMirrorWindowOuterFlangeMaterial;	
  G4Material* fInterfaceRingMaterial;

  G4double  fMirrorOpticalSurfaceSigmaAlpha;
  G4double* fMirrorReflectivity;
  G4double* fMirrorEfficiency;

  G4double* fConeReflectivity;
  G4double* fConeEfficiency;

  G4double  fPMTsWindowOpticalSurfaceSigmaAlpha;
  G4double* fPMTsWindowReflectivity;
  G4double* fPMTsWindowEfficiency;
  G4double* fPMTsWindowSpecularSpike;
  G4double* fPMTsWindowSpecularLobe;
  G4double* fPMTsWindowBackscatter;

  G4double  fVesselOpticalSurfaceSigmaAlpha;
  G4double* fVesselReflectivity;
  G4double* fVesselEfficiency;
  G4double* fVesselSpecularSpike;
  G4double* fVesselSpecularLobe;
  G4double* fVesselBackscatter;

  G4String* fPMType;
  G4double* fPMPeakQE;
  G4double** fPMQuantumEfficiency;

  G4MaterialPropertiesTable* fNeonMPT;
  G4MaterialPropertiesTable* fQuartzMPT;
  G4MaterialPropertiesTable* fVacuumMPT;

  G4MaterialPropertiesTable* fMirrorOpticalSurfacePT;
  G4MaterialPropertiesTable* fConeOpticalSurfacePT;
  G4MaterialPropertiesTable* fPMTsWindowOpticalSurfacePT;
  G4MaterialPropertiesTable* fVesselOpticalSurfacePT;

  G4MaterialPropertiesTable** fPhotocatodePT;

};
#endif
