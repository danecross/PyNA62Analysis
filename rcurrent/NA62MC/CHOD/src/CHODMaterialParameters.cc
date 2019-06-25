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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------

#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"
#include "CHODMaterialParameters.hh"

CHODMaterialParameters* CHODMaterialParameters::fInstance = 0;

CHODMaterialParameters::CHODMaterialParameters() {
  // Prepare here every Material Property Table
  DefineMaterials();
  SetMaterialProperties();
}

CHODMaterialParameters* CHODMaterialParameters::GetInstance() {
  if (!fInstance) { fInstance = new CHODMaterialParameters(); }
  return fInstance;
}

void CHODMaterialParameters::DefineMaterials() {

  // This method creates a set of materials which will be used to build
  // the detector. It uses the predefined NIST-derived materials list
  // and adds. If you plan to change or add some property to on material
  // you need to create it as a copy of a NIST-derived to avoid conflict
  // with other subdetectors

  G4NistManager* nistMgr = G4NistManager::Instance();
  nistMgr->FindOrBuildMaterial("G4_Galactic");
  nistMgr->FindOrBuildMaterial("G4_PLASTIC_SC_VINYLTOLUENE");
}

void CHODMaterialParameters::SetMaterialProperties() {
  // Use here the material property tables prepared
}

TObjArray CHODMaterialParameters::GetHashTable() {
  return 0;
}

void CHODMaterialParameters::Print() {}
