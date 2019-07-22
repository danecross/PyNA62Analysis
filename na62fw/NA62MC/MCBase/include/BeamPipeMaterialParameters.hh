//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (francesca.bucci@cern.ch) 2011-03-11
//
// --------------------------------------------------------------
#ifndef BeamPipeMaterialParameters_H
#define BeamPipeMaterialParameters_H 1

#include "globals.hh"
#include "G4SystemOfUnits.hh"
#include "G4PhysicalConstants.hh"
#include "TObjArray.h"

class BeamPipeMaterialParameters
{

public:

  ~BeamPipeMaterialParameters();
  static BeamPipeMaterialParameters* GetInstance();

private:

  static BeamPipeMaterialParameters* fInstance;

protected:

  BeamPipeMaterialParameters();
  void DefineMaterials();
  void SetMaterialProperties();

public:

  G4double             GetTubeOpticalSurfaceSigmaAlpha()                  { return fTubeOpticalSurfaceSigmaAlpha; };
  void                 SetTubeOpticalSurfaceSigmaAlpha(G4double value)    { fTubeOpticalSurfaceSigmaAlpha = value; };
  G4double *           GetTubeReflectivity()                              { return fTubeReflectivity;             };
  void                 SetTubeReflectivity(G4double * value)              { fTubeReflectivity = value;            };
  G4double *           GetTubeEfficiency()                                { return fTubeEfficiency;               };
  void                 SetTubeEfficiency(G4double * value)                { fTubeEfficiency = value;              };
  G4double *           GetTubeSpecularSpike()                             { return fTubeSpecularSpike;            };
  void                 SetTubeSpecularSpike(G4double * value)             { fTubeSpecularSpike = value;           };
  G4double *           GetTubeSpecularLobe()                              { return fTubeSpecularLobe;             };
  void                 SetTubeSpecularLobe(G4double * value)              { fTubeSpecularLobe = value;            };
  G4double *           GetTubeBackscatter()                               { return fTubeBackscatter;              };
  void                 SetTubeBackscatter(G4double * value)               { fTubeBackscatter = value;             };

  G4MaterialPropertiesTable *
                       GetTubeOpticalSurfacePT()                          { return fTubeOpticalSurfacePT;         };
  void                 SetTubeOpticalSurfacePT(G4MaterialPropertiesTable * value)
                                                                          { fTubeOpticalSurfacePT = value;        };

private:
  
  G4int fMaterialPropertiesNEntries;

  G4double* fPhotonEnergy;

  G4double  fTubeOpticalSurfaceSigmaAlpha;
  G4double* fTubeReflectivity;
  G4double* fTubeEfficiency;
  G4double* fTubeSpecularSpike;
  G4double* fTubeSpecularLobe;
  G4double* fTubeBackscatter;

  G4MaterialPropertiesTable* fTubeOpticalSurfacePT;

};
#endif
