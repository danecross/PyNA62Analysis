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
// 2012-02-29 B.Velghe
// - Add Epoxy resin (CMS IN1999/026)
//
// 2012-01-18 B.Velghe
// - Add C6F14 coolant
//
// 2008-04-22 S.Bifani (Simone.Bifani@cern.ch)
// - added Sn-Pb bump bonding and carbon materials
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// --------------------------------------------------------------
//
#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"

#include "GigaTrackerMaterialParameters.hh"
#include "GigaTrackerGeometryParameters.hh"

#include "DetectorParameter.hh"
#include "TVector.h"


GigaTrackerMaterialParameters* GigaTrackerMaterialParameters::fInstance = 0;

GigaTrackerMaterialParameters::GigaTrackerMaterialParameters()
{

  // Prepare here every Material Property Table

  DefineMaterials();
  SetMaterialProperties();

}

GigaTrackerMaterialParameters::~GigaTrackerMaterialParameters() {}

GigaTrackerMaterialParameters* GigaTrackerMaterialParameters::GetInstance()
{

  if ( fInstance == 0 ) fInstance = new GigaTrackerMaterialParameters();
  return fInstance;

}

void GigaTrackerMaterialParameters::DefineMaterials()
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

  nistMgr->FindOrBuildMaterial("G4_Al");
  nistMgr->FindOrBuildMaterial("G4_Fe");
  nistMgr->FindOrBuildMaterial("G4_Kr");
  nistMgr->FindOrBuildMaterial("G4_Pb");
  nistMgr->FindOrBuildMaterial("G4_Si");
  nistMgr->FindOrBuildMaterial("G4_Sn");
  nistMgr->FindOrBuildMaterial("G4_Ag");
  nistMgr->FindOrBuildMaterial("G4_MYLAR");
  nistMgr->FindOrBuildMaterial("G4_SILICON_DIOXIDE");
  nistMgr->FindOrBuildMaterial("G4_PLASTIC_SC_VINYLTOLUENE");
  nistMgr->FindOrBuildMaterial("G4_WATER");
  nistMgr->FindOrBuildMaterial("G4_POLYVINYL_CHLORIDE");
  nistMgr->FindOrBuildMaterial("G4_GRAPHITE");
  nistMgr->FindOrBuildMaterial("G4_STAINLESS-STEEL");

  // Materials to be specific for this subdetector
  G4Material* BumpBonding = new G4Material("GTK_BumpBonding", 8.46 *g/cm3, 2); // http://www.springerlink.com/content/28403q2090065770/
  BumpBonding->AddMaterial(G4Material::GetMaterial("G4_Sn"), 96.5*perCent);
  BumpBonding->AddMaterial(G4Material::GetMaterial("G4_Ag"), 3.5*perCent);


  //Coolant
  G4Material * Coolant = new G4Material("GTK_Coolant",1.805 *g/cm3, 2); //density, ncomponents
  G4Element * C = nistMgr->FindOrBuildElement("C");
  G4Element * F = nistMgr->FindOrBuildElement("F");
  Coolant->AddElement(C,6); //natoms
  Coolant->AddElement(F,14);

  //Epoxy resin (CMS IN1999/026) 
  G4Material * Epoxy = new G4Material("GTK_Glue",1.3 *g/cm3, 3); //density, ncomponents
  G4Element * H = nistMgr->FindOrBuildElement("H");
  G4Element * O = nistMgr->FindOrBuildElement("O");
  Epoxy->AddElement(C,15);
  Epoxy->AddElement(H,44);
  Epoxy->AddElement(O,7);

  G4Material * SiO_2 = new G4Material("SiO_2",2.2*g/cm3,2);
  G4Element * Si = nistMgr->FindOrBuildElement("Si");
  SiO_2->AddElement(Si,1);
  SiO_2->AddElement(O,2);

  //NEMA G10
  G4Material * NEMA_G10 = new G4Material("GTK_PCB",1.7*g/cm3,2);
  NEMA_G10->AddMaterial(Epoxy,0.25); // ! GTK_Glue == Epoxy !
  NEMA_G10->AddMaterial(SiO_2,0.75);


  //nistMgr->RegisterMaterial(StainlessSteel);

}

void GigaTrackerMaterialParameters::SetMaterialProperties()
{

  // Use here the material property tables prepared

}

TObjArray GigaTrackerMaterialParameters::GetHashTable()
{

  TObjArray GigaTrackerMaterialParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fMaterialPropertiesNEntries;
  Value = Buffer.str();
  Buffer.str("");
  Float_t MaterialPropertiesNEntries = fMaterialPropertiesNEntries;
  ParameterData.Add(new TVector(1, &MaterialPropertiesNEntries));
  GigaTrackerMaterialParameters.Add(new DetectorParameter("fMaterialPropertiesNEntries",Value.Data(),
							  "Material Properties N Entries", ParameterData));
  ParameterData.Clear();

  return GigaTrackerMaterialParameters;

}

void GigaTrackerMaterialParameters::Print(){

  G4cout << "fMaterialPropertiesNEntries= " << fMaterialPropertiesNEntries << G4endl;

}




