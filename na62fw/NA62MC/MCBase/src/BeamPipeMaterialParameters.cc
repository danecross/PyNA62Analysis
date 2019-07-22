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
#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"

#include "TVector.h"
#include "TObjString.h"

#include "BeamPipeMaterialParameters.hh"
#include "DetectorParameter.hh"

#ifndef BEAMPIPER
#define BEAMPIPER 0.70
#endif
#ifndef BEAMPIPESp
#define BEAMPIPESp 0.
#endif
#ifndef BEAMPIPEL
#define BEAMPIPEL 0.75
#endif
#ifndef BEAMPIPES
#define BEAMPIPES 0.070
#endif

BeamPipeMaterialParameters* BeamPipeMaterialParameters::fInstance = 0;

BeamPipeMaterialParameters::BeamPipeMaterialParameters() :
   fMaterialPropertiesNEntries(50)
{

   fPhotonEnergy = new G4double[fMaterialPropertiesNEntries];
   fTubeReflectivity = new G4double[fMaterialPropertiesNEntries];
   fTubeEfficiency = new G4double[fMaterialPropertiesNEntries];
   fTubeSpecularSpike = new G4double[fMaterialPropertiesNEntries];
   fTubeSpecularLobe = new G4double[fMaterialPropertiesNEntries];
   fTubeBackscatter = new G4double[fMaterialPropertiesNEntries];
 
   for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){

     fPhotonEnergy[i]=2.00*eV + (7.75 - 2.00)*eV / (fMaterialPropertiesNEntries - 1)* i;
     
     fTubeReflectivity[i] = BEAMPIPER; //TubeRefl
     fTubeEfficiency[i] = 0.;
     fTubeSpecularSpike[i] = BEAMPIPESp; //TubeSpike
     fTubeSpecularLobe[i] = BEAMPIPEL; //TubeLobe
     fTubeBackscatter[i] = 0.; //TubeBS 
  
   }  
 
    fTubeOpticalSurfaceSigmaAlpha = BEAMPIPES*rad; //TubeSigma
    fTubeOpticalSurfacePT = new G4MaterialPropertiesTable();
    fTubeOpticalSurfacePT->AddProperty("REFLECTIVITY",fPhotonEnergy,
   				     fTubeReflectivity,fMaterialPropertiesNEntries);
    fTubeOpticalSurfacePT->AddProperty("EFFICIENCY",fPhotonEnergy,
				       fTubeEfficiency,fMaterialPropertiesNEntries);
    fTubeOpticalSurfacePT->AddProperty("SPECULARSPIKECONSTANT",fPhotonEnergy, 
				       fTubeSpecularSpike, fMaterialPropertiesNEntries);
    fTubeOpticalSurfacePT->AddProperty("SPECULARLOBECONSTANT", fPhotonEnergy, 
				       fTubeSpecularLobe,  fMaterialPropertiesNEntries);
    fTubeOpticalSurfacePT->AddProperty("BACKSCATTERCONSTANT",  fPhotonEnergy, 
				       fTubeBackscatter,   fMaterialPropertiesNEntries);


  DefineMaterials();
  SetMaterialProperties();
  
}

BeamPipeMaterialParameters::~BeamPipeMaterialParameters(){}

BeamPipeMaterialParameters* BeamPipeMaterialParameters::GetInstance()
{
  if ( fInstance == 0 ) { fInstance = new BeamPipeMaterialParameters(); }
  return fInstance;
}


void BeamPipeMaterialParameters::DefineMaterials()
{

}

void BeamPipeMaterialParameters::SetMaterialProperties()
{

}


