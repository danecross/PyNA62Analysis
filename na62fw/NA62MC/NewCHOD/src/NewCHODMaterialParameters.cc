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
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// --------------------------------------------------------------------

#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"
#include "NewCHODMaterialParameters.hh"

NewCHODMaterialParameters* NewCHODMaterialParameters::fInstance = 0;

NewCHODMaterialParameters::NewCHODMaterialParameters() {
  // Prepare here every Material Property Table
  DefineMaterials();
  SetMaterialProperties();
}

NewCHODMaterialParameters* NewCHODMaterialParameters::GetInstance() {
  if (!fInstance) fInstance = new NewCHODMaterialParameters();
  return fInstance;
}

void NewCHODMaterialParameters::DefineMaterials() {

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
  nistMgr->FindOrBuildMaterial("G4_Galactic");
  nistMgr->FindOrBuildMaterial("G4_Al");
  nistMgr->FindOrBuildMaterial("G4_PLASTIC_SC_VINYLTOLUENE");

  //////////////////////////////////////////////
  // Build the G10 material used for the NewCHOD

  G4Element* H  = nistMgr->FindOrBuildElement( 1);
  G4Element* C  = nistMgr->FindOrBuildElement( 6);
  G4Element* O  = nistMgr->FindOrBuildElement( 8);
  G4Element* Si = nistMgr->FindOrBuildElement(14);
  G4int natoms = 0;
  G4Material* G10 = new G4Material("NemaG10", 2.0*g/cm3, 4); // density: Italo, 18 Nov 2015
  G10->AddElement(Si, natoms=1);
  G10->AddElement(O,  natoms=2);
  G10->AddElement(C,  natoms=3);
  G10->AddElement(H,  natoms=3);
}

void NewCHODMaterialParameters::SetMaterialProperties() {
  // Use here the material property tables prepared
}

TObjArray NewCHODMaterialParameters::GetHashTable() {
  return 0;
}

void NewCHODMaterialParameters::Print() {}
