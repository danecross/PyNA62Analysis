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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-12
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
//
//rubber material added by ykohl in March 2010
//
// --------------------------------------------------------------------
#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"

#include "TVector.h"

#include "MUV2MaterialParameters.hh"
#include "MUV2GeometryParameters.hh"
#include "DetectorParameter.hh"

MUV2MaterialParameters* MUV2MaterialParameters::fInstance = 0;

MUV2MaterialParameters::MUV2MaterialParameters() :
 fMaterialPropertiesNEntries(0),
 fPhotonEnergy(nullptr),
 fNeonRefIndex(nullptr),
 fNeonScintilFast(nullptr),
 fNeonAbsorptionLength(nullptr),
 fNeonMPT(nullptr)
{
  // Prepare here every Material Property Table
  DefineMaterials();
  SetMaterialProperties();
}

MUV2MaterialParameters::~MUV2MaterialParameters(){}

MUV2MaterialParameters* MUV2MaterialParameters::GetInstance()
{
  if ( fInstance == 0 )
    fInstance = new MUV2MaterialParameters();

  return fInstance;
}

void MUV2MaterialParameters::DefineMaterials()
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
  nistMgr->FindOrBuildMaterial("G4_RUBBER_NATURAL");		//necessary for rubber plates!!
  nistMgr->FindOrBuildMaterial("G4_POLYSTYRENE");
  nistMgr->FindOrBuildMaterial("G4_POLYPROPYLENE");

  //Define POPOP
  std::vector<G4String> POPOPEl(4);
  std::vector<G4int> POPOPNA(4);
  POPOPEl[0] = "C"; POPOPNA[0] = 24;
  POPOPEl[1] = "O"; POPOPNA[1] = 2;
  POPOPEl[2] = "H"; POPOPNA[2] = 16;
  POPOPEl[3] = "N"; POPOPNA[3] = 2;
  nistMgr->ConstructNewMaterial("POPOP_2",POPOPEl,POPOPNA,1.204,true);

  // Define p-Terphenyl
  std::vector<G4String> pTerphenylEl(2);
  std::vector<G4int> pTerphenylNA(2);
  pTerphenylEl[0] = "C"; pTerphenylNA[0] = 18;
  pTerphenylEl[1] = "H"; pTerphenylNA[1] = 14;
  nistMgr->ConstructNewMaterial("pTerphenyl_2",pTerphenylEl,pTerphenylNA,1.23,true);

 
  //Define Scintillator material
  G4Material* Scint = new G4Material("Scint_2",1.06*g/cm3,3);
  Scint->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 97.7*perCent); // 98%
  Scint->AddMaterial(G4Material::GetMaterial("POPOP_2"),  0.3*perCent); // 0.3%
  Scint->AddMaterial(G4Material::GetMaterial("pTerphenyl_2"),  2.0*perCent); // 2%

  // Add optical properties to Scint

  // Number of bins for Scintillator material properties table
  const G4int nEntries = 10;

  G4double PhotonEnergy[nEntries] =
    { 1.88914*eV, 1.92582*eV, 1.95929*eV, 2.10392*eV, 2.11001*eV,
      2.27035*eV, 2.55059*eV, 2.58300*eV, 2.84497*eV, 3.06360*eV};

  G4double RefractiveIndexScint[nEntries] =
    { 1.6,    1.83808,    1.83957,    1.84636,    1.84666,
      1.85504,    1.87204,    1.87425,    1.89393,    1.91366 };

  // Abosrption lengths as declared by provider
  G4double AbsorptionScint[nEntries] =
    {   420.*cm,    420.*cm,    420.*cm,    420.*cm,    420.*cm,
        420.*cm,    144.*cm,    130.*cm,     34.*cm,      8.*cm };
  //G4double EmissionScint[nEntries]={0.0, 0.0, 0.0, 0.1, 0.5, 1.0, 5.0, 5.0, 10.0, 10.0};


  //G4double ScintilFast[nEntries] =
  //          { 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00 };
  //G4double ScintilSlow[nEntries] =
  //          { 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00 };


  // Define property table for Scint
  G4MaterialPropertiesTable* myMPTScint = new G4MaterialPropertiesTable();
  myMPTScint->AddProperty("RINDEX",PhotonEnergy, RefractiveIndexScint, nEntries);
  myMPTScint->AddProperty("ABSLENGTH",PhotonEnergy, AbsorptionScint, nEntries);
  //myMPTScint->AddProperty("COMPONENT",PhotonEnergy, EmissionScint, nEntries);
  //myMPTScint->AddConstProperty("TIMECONSTANT",0.5*ns);
  
  //myMPTPbGl->AddProperty("FASTCOMPONENT",PhotonEnergy, ScintilFast, nEntries);
  //myMPTPbGl->AddProperty("SLOWCOMPONENT",PhotonEnergy, ScintilSlow, nEntries);

  //myMPTScint->AddConstProperty("SCINTILLATIONYIELD",100./eV);
  //myMPTScint->AddConstProperty("RESOLUTIONSCALE",0.75);
  //myMPTPbGl->AddConstProperty("FASTTIMECONSTANT", 0.*ns);
  //myMPTPbGl->AddConstProperty("SLOWTIMECONSTANT",0.*ns);
  //myMPTPbGl->AddConstProperty("YIELDRATIO",0.);
  
  Scint->SetMaterialPropertiesTable(myMPTScint);

  // Materials to be specific for this subdetector
  // Example: Stainless Steel (StainlessSteel)
  G4Material* StainlessSteel = new G4Material("MUV2_StainlessSteel",7.88*g/cm3,4); //Prepend subdetector name
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Fe"), 71.5*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Cr"), 18.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Ni"), 10.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Si"),  0.5*perCent); 

  //nistMgr->RegisterMaterial(StainlessSteel);
}

void MUV2MaterialParameters::SetMaterialProperties()
{
  // Use here the material property tables prepared
}

TObjArray MUV2MaterialParameters::GetHashTable()
{
  TObjArray MUV2MaterialParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fMaterialPropertiesNEntries;
  Value = Buffer.str();
  Buffer.str("");
  Float_t MaterialPropertiesNEntries = fMaterialPropertiesNEntries;

  ParameterData.Add(new TVector(1, &MaterialPropertiesNEntries));
  MUV2MaterialParameters.Add(new DetectorParameter("fMaterialPropertiesNEntries",Value.Data(),
						   "Material Properties N Entries", ParameterData));
  ParameterData.Clear();

  return MUV2MaterialParameters;
}

void MUV2MaterialParameters::Print()
{
  G4cout << "fMaterialPropertiesNEntries= "<< fMaterialPropertiesNEntries << G4endl;
}
