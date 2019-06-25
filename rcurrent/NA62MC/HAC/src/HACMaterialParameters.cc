#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"

#include "TVector.h"

#include "HACMaterialParameters.hh"
#include "HACGeometryParameters.hh"
#include "DetectorParameter.hh"
//
// --------------------------------------------------------------------
// History:
//
// Created by Giuseppe Ruggiero 04-09-2012 
//
// --------------------------------------------------------------------

HACMaterialParameters* HACMaterialParameters::fInstance = 0;

HACMaterialParameters::HACMaterialParameters()
{
  // Prepare here every Material Property Table

  DefineMaterials();
  SetMaterialProperties();
}

HACMaterialParameters::~HACMaterialParameters(){}

HACMaterialParameters* HACMaterialParameters::GetInstance()
{
  if ( fInstance == 0 ) { fInstance = new HACMaterialParameters(); }
  return fInstance;
}

void HACMaterialParameters::DefineMaterials()
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
  G4Material* StainlessSteel = new G4Material("HAC_StainlessSteel",7.88*g/cm3,4); //Prepend subdetector name
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Fe"), 71.5*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Cr"), 18.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Ni"), 10.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Si"),  0.5*perCent); 

  //nistMgr->RegisterMaterial(StainlessSteel);

}

void HACMaterialParameters::SetMaterialProperties()
{
  // Use here the material property tables prepared
}

TObjArray HACMaterialParameters::GetHashTable()
{
  TObjArray HACMaterialParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fMaterialPropertiesNEntries;
  Value = Buffer.str();
  Buffer.str("");
  Float_t MaterialPropertiesNEntries = fMaterialPropertiesNEntries;
  ParameterData.Add(new TVector(1, &MaterialPropertiesNEntries));
  HACMaterialParameters.Add(new DetectorParameter("fMaterialPropertiesNEntries",Value.Data(),
					       "Material Properties N Entries", ParameterData));
  ParameterData.Clear();


  return HACMaterialParameters;
}

void HACMaterialParameters::Print(){
  G4cout << "fMaterialPropertiesNEntries= "<< fMaterialPropertiesNEntries << G4endl;
}
