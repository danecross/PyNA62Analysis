// --------------------------------------------------------------------
// History:
//
// Created by Giuseppe Ruggiero 04-09-2012 
//
// --------------------------------------------------------------------

#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"
#include "MUV0MaterialParameters.hh"

MUV0MaterialParameters* MUV0MaterialParameters::fInstance = 0;

MUV0MaterialParameters::MUV0MaterialParameters() {
  DefineMaterials();
  SetMaterialProperties();
}

MUV0MaterialParameters* MUV0MaterialParameters::GetInstance() {
  if (!fInstance) fInstance = new MUV0MaterialParameters();
  return fInstance;
}

void MUV0MaterialParameters::DefineMaterials() {
  G4NistManager* nistMgr = G4NistManager::Instance();

  // Use these if you want extensive output about materials definition
  //nistMgr->SetVerbose(1);
  //nistMgr->PrintElement("all");
  //nistMgr->ListMaterials("all");

  // Standard materials
  nistMgr->FindOrBuildMaterial("G4_Galactic");
  nistMgr->FindOrBuildMaterial("G4_Fe");
  nistMgr->FindOrBuildMaterial("G4_Cr");
  nistMgr->FindOrBuildMaterial("G4_Ni");
  nistMgr->FindOrBuildMaterial("G4_Si");
  nistMgr->FindOrBuildMaterial("G4_Al");

  // Materials to be specific for this subdetector
  G4Material* StainlessSteel = new G4Material("MUV0_StainlessSteel", 7.88*g/cm3, 4);
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Fe"), 71.5*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Cr"), 18.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Ni"), 10.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Si"),  0.5*perCent); 

  //nistMgr->RegisterMaterial(StainlessSteel);
}

void MUV0MaterialParameters::SetMaterialProperties() {
  // Use here the material property tables prepared
}

TObjArray MUV0MaterialParameters::GetHashTable() {
  return 0;
}
