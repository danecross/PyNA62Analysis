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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"

#include "TVector.h"

#include "SpectrometerMaterialParameters.hh"
#include "SpectrometerGeometryParameters.hh"
#include "DetectorParameter.hh"

SpectrometerMaterialParameters* SpectrometerMaterialParameters::fInstance = 0;

SpectrometerMaterialParameters::SpectrometerMaterialParameters()
{
  // Prepare here every Material Property Table

  DefineMaterials();
  SetMaterialProperties();
}

SpectrometerMaterialParameters::~SpectrometerMaterialParameters(){}

SpectrometerMaterialParameters* SpectrometerMaterialParameters::GetInstance()
{
  if ( fInstance == 0 ) { fInstance = new SpectrometerMaterialParameters(); }
  return fInstance;
}

void SpectrometerMaterialParameters::DefineMaterials()
{
  G4String symbol;
  G4double a,z,density;  
  G4int    ncomponents;
  G4double fractionmass;
  //G4double temperature, pressure;

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

  nistMgr->FindOrBuildMaterial("G4_Cu");
  nistMgr->FindOrBuildMaterial("G4_Au");
  nistMgr->FindOrBuildMaterial("G4_MYLAR");
  nistMgr->FindOrBuildMaterial("G4_Ar");
  nistMgr->FindOrBuildMaterial("G4_CARBON_DIOXIDE");

  //G4Element* C = new G4Element("Carbon",symbol="C",z=6,a=12.01*g/mole);
  //G4Element* H = new G4Element("Hydrogen",symbol="H",z=1,a=1.01*g/mole);
  //G4Element* O = new G4Element("Oxygen",symbol="O",z=8,a=16.00*g/mole);
  //G4Element* F = new G4Element("Fluorine",symbol="F",z=9,a=18.9984032*g/mole);
  G4Material* W  = new G4Material("Tungsten",z=74,a=183.85*g/mole,density=19.30*g/cm3);
  G4Material* Re = new G4Material("Rhenium",z=75,a=186.207*g/mole,density=21.02*g/cm3);

  //nistMgr->RegisterMaterial(StainlessSteel);
  //G4Material* CO2 = new G4Material("CarbonicGas",density=1.87*mg/cm3,ncomponents=2,kStateGas,288.15*kelvin,1*atmosphere);
  //CO2->AddElement(C, natoms=1);
  //CO2->AddElement(O, natoms=2);

  //G4Material *CF4 = new G4Material("TetraFluoroMethane",density=3.72*mg/cm3,ncomponents=2,kStateGas,288.15*kelvin,1*atmosphere);
  //CF4->AddElement(C,natoms=1);
  //CF4->AddElement(F,natoms=2); 
  //
  //G4Material *C4H10 = new G4Material("Isobuthane",density=2.51*mg/cm3,ncomponents=2,kStateGas,288.15*kelvin,1*atmosphere);
  //C4H10->AddElement(C,natoms=4);
  //C4H10->AddElement(H,natoms=10); 

  //G4Material* GasStraw = new G4Material("GasStraw",density=2.118*mg/cm3,ncomponents=3,kStateGas,288.15*kelvin,1*atmosphere);
  //GasStraw->AddMaterial(CO2,fractionmass=0.80);
  //GasStraw->AddMaterial(CF4,fractionmass=0.10);
  //GasStraw->AddMaterial(C4H10,fractionmass=0.10);

  G4Material* GasStraw = new G4Material("GasStraw",density=1.977*mg/cm3,ncomponents=2,kStateGas,288.15*kelvin,1*atmosphere);
  GasStraw->AddMaterial(G4Material::GetMaterial("G4_Ar"),fractionmass=0.70);
  GasStraw->AddMaterial(G4Material::GetMaterial("G4_CARBON_DIOXIDE"),fractionmass=0.30);

  G4Material* Luma861 = new G4Material("Luma861",density=19.3516*g/cm3,ncomponents=2);
  Luma861->AddMaterial(W,fractionmass=0.97);
  Luma861->AddMaterial(Re,fractionmass=0.03);
}

void SpectrometerMaterialParameters::SetMaterialProperties()
{
  // Use here the material property tables prepared
}

TObjArray SpectrometerMaterialParameters::GetHashTable()
{
  TObjArray SpectrometerMaterialParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fMaterialPropertiesNEntries;
  Value = Buffer.str();
  Buffer.str("");
  Float_t MaterialPropertiesNEntries = fMaterialPropertiesNEntries;
  ParameterData.Add(new TVector(1, &MaterialPropertiesNEntries));
  SpectrometerMaterialParameters.Add(new DetectorParameter("fMaterialPropertiesNEntries",Value.Data(),
					       "Material Properties N Entries", ParameterData));
  ParameterData.Clear();


  return SpectrometerMaterialParameters;
}

void SpectrometerMaterialParameters::Print(){
  G4cout << "fMaterialPropertiesNEntries= "<< fMaterialPropertiesNEntries << G4endl;
}

