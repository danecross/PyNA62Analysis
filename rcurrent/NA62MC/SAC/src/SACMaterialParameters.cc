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
// --------------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 
//	      Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------
#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"

#include "TVector.h"

#include "SACMaterialParameters.hh"
#include "SACGeometryParameters.hh"
#include "DetectorParameter.hh"

SACMaterialParameters* SACMaterialParameters::fInstance = 0;

SACMaterialParameters::SACMaterialParameters()
{
  // Prepare here every Material Property Table

  DefineMaterials();
  SetMaterialProperties();
}

SACMaterialParameters::~SACMaterialParameters(){}

SACMaterialParameters* SACMaterialParameters::GetInstance()
{
  if ( fInstance == 0 ) { fInstance = new SACMaterialParameters(); }
  return fInstance;
}

void SACMaterialParameters::DefineMaterials()
{

  // This method creates a set of materials which will be used to build
  // the detector. It uses the predefined NIST-derived materials list
  // and adds. If you plan to change or add some property to on material
  // you need to create it as a copy of a NIST-derived to avoid conflict
  // with other subdetectors

  G4NistManager* nistMgr = G4NistManager::Instance();

  // Use these if you want extensive output about materials definition
  //nistMgr->SetVerbose(1);
  //nistMgr->PrintElement("all");
  //nistMgr->ListMaterials("all");

  // Standard materials
  nistMgr->FindOrBuildMaterial("G4_AIR");
  nistMgr->FindOrBuildMaterial("G4_Galactic");

  nistMgr->FindOrBuildMaterial("G4_Fe");
  nistMgr->FindOrBuildMaterial("G4_Pb");
  nistMgr->FindOrBuildMaterial("G4_Ni");
  nistMgr->FindOrBuildMaterial("G4_Si");
  nistMgr->FindOrBuildMaterial("G4_Cr");
  nistMgr->FindOrBuildMaterial("G4_Al");
  nistMgr->FindOrBuildMaterial("G4_Kr");
  nistMgr->FindOrBuildMaterial("G4_MYLAR");
  nistMgr->FindOrBuildMaterial("G4_SILICON_DIOXIDE");
  nistMgr->FindOrBuildMaterial("G4_PLASTIC_SC_VINYLTOLUENE");
  nistMgr->FindOrBuildMaterial("G4_WATER");
  nistMgr->FindOrBuildMaterial("G4_POLYVINYL_CHLORIDE");
  nistMgr->FindOrBuildMaterial("G4_POLYSTYRENE");

  // Materials to be specific for this subdetector
  // Example: Stainless Steel (StainlessSteel)
  G4Material* StainlessSteel = new G4Material("SAC_StainlessSteel",7.88*g/cm3,4); //Prepend subdetector name
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Fe"), 71.5*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Cr"), 18.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Ni"), 10.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Si"),  0.5*perCent); 

  // Define 96% Pb + 4% Sn for SAC absorber
  // Density calculated to be: 0.04 * 7.365 + 0.96 * 11.34 = 11.181 g/cm3
  G4Material* PbSnAlloy = new G4Material("SAC_PbSnAlloy",11.181*g/cm3,2);
  PbSnAlloy->AddMaterial(G4Material::GetMaterial("G4_Pb"),96*perCent);
  PbSnAlloy->AddMaterial(G4Material::GetMaterial("G4_Sn"),4*perCent);

  //nistMgr->RegisterMaterial(StainlessSteel);

}

void SACMaterialParameters::SetMaterialProperties()
{
  // Use here the material property tables prepared
}

TObjArray SACMaterialParameters::GetHashTable()
{
  TObjArray SACMaterialParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fMaterialPropertiesNEntries;
  Value = Buffer.str();
  Buffer.str("");
  Float_t MaterialPropertiesNEntries = fMaterialPropertiesNEntries;
  ParameterData.Add(new TVector(1, &MaterialPropertiesNEntries));
  SACMaterialParameters.Add(new DetectorParameter("fMaterialPropertiesNEntries",Value.Data(),
					       "Material Properties N Entries", ParameterData));
  ParameterData.Clear();


  return SACMaterialParameters;
}

void SACMaterialParameters::Print(){
  G4cout << "fMaterialPropertiesNEntries= "<< fMaterialPropertiesNEntries << G4endl;
}
