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
//            Evelina MArinova(Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------
#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"

#include "TVector.h"

#include "LKrMaterialParameters.hh"
#include "LKrGeometryParameters.hh"
#include "DetectorParameter.hh"

/// \class LKrMaterialParameters 
/// \Brief
/// LKrMaterialParameters class.
/// \EndBrief   
///
/// \Detailed
/// This class stores and provides the information about the materials used in the LKr simulation.
/// \EndDetailed


LKrMaterialParameters* LKrMaterialParameters::fInstance = 0;

LKrMaterialParameters::LKrMaterialParameters()
{
    // Prepare here every Material Property Table

    DefineMaterials();
    SetMaterialProperties();
}

LKrMaterialParameters::~LKrMaterialParameters(){}

LKrMaterialParameters* LKrMaterialParameters::GetInstance()
{
    if ( fInstance == 0 ) { fInstance = new LKrMaterialParameters(); }
    return fInstance;
}

void LKrMaterialParameters::DefineMaterials()
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
    nistMgr->FindOrBuildMaterial("G4_Cu");
    nistMgr->FindOrBuildMaterial("G4_Zn");
    nistMgr->FindOrBuildMaterial("G4_Al");
    nistMgr->FindOrBuildMaterial("G4_lKr");
    nistMgr->FindOrBuildMaterial("G4_MYLAR");
    nistMgr->FindOrBuildMaterial("G4_SILICON_DIOXIDE");
    nistMgr->FindOrBuildMaterial("G4_PLASTIC_SC_VINYLTOLUENE"); /// Scintillator
    nistMgr->FindOrBuildMaterial("G4_WATER");
    nistMgr->FindOrBuildMaterial("G4_POLYVINYL_CHLORIDE");  ///PVC
    nistMgr->FindOrBuildMaterial("G4_GLASS_PLATE");
    nistMgr->FindOrBuildMaterial("G4_H");
    nistMgr->FindOrBuildMaterial("G4_O");
    nistMgr->FindOrBuildMaterial("G4_C");
    nistMgr->FindOrBuildMaterial("G4_Be");
    nistMgr->FindOrBuildMaterial("G4_Co");
    nistMgr->FindOrBuildMaterial("G4_TEFLON");

    // Materials to be specific for this subdetector
    // Example: Stainless Steel (StainlessSteel)

    G4Material* StainlessSteel = new G4Material("LKr_StainlessSteel",7.88*g/cm3,4); //Prepend subdetector name
    StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Fe"), 71.5 *perCent); 
    StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Cr"), 18.0 *perCent); 
    StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Ni"), 10.0 *perCent); 
    StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Si"),  0.5 *perCent);


    G4Material* LKrRibbon = new G4Material("LKr_Ribbon", 8750 * kg/m3, 3); //Prepend subdetector name // density taken from na48 documentation.
    LKrRibbon->AddMaterial(G4Material::GetMaterial("G4_Cu"), 98.0 *perCent); //8.9 g.cm-3 at 20°C
    LKrRibbon->AddMaterial(G4Material::GetMaterial("G4_Be"), 1.8 *perCent); //1.86 g.cm-3
    LKrRibbon->AddMaterial(G4Material::GetMaterial("G4_Co"), 0.2 *perCent);//8.9 g.cm-3 at 20°C 

    G4Material* LKrBrass = new G4Material("LKr_Brass", 8700 * kg/m3, 2); //Prepend subdetector name // density & % in the mixture taken from wiki.
    LKrBrass->AddMaterial(G4Material::GetMaterial("G4_Cu"), 65.0 *perCent); //
    LKrBrass->AddMaterial(G4Material::GetMaterial("G4_Zn"), 35.0 *perCent); //

    //Define Material GTEN - epoxy  ( C8 H14 O4 ) 
    //  
    //density = 1.8 *g/cm3; 

    G4double a, z;

    // Hydrogen 
    a = 1.01 *g/mole; 
    G4Element *elH = new G4Element("Hydrogen", "H", z = 1., a); 


    // Carbon 
    a = 12.01 *g/mole; 
    G4Element *elC = new G4Element("Carbon", "C", z = 6., a); 


    // Oxygen 
    a = 16.00 *g/mole; 
    G4Element *elO = new G4Element("Oxygen", "O", z = 8., a); 



    G4int natoms;
    G4double density_epoxy = 1.8 *g/cm3;
    G4Material* LKrEpoxy = new G4Material("LKr_Epoxy", density_epoxy, 3); //Prepend subdetector name


    LKrEpoxy->AddElement(elH, natoms = 14); 
    LKrEpoxy->AddElement(elO, natoms = 4); 
    LKrEpoxy->AddElement(elC, natoms = 8); 



    /////// aluminium honeycomb material - aluminium + vacuum (this structure is vacuumed separately from the LKr Cryo)
    ///  hexagon structure, the hexagon is 6 mm wide, and the hexagon wall is 60 micro m thick
    /// keep in mind that one wall is shared between 2 hexagon cells , so 0.6 / 5.4 gives ~ 11% aluminium



    G4Material* LKrAluminiumHoneycombFillerMaterial = new G4Material("LKr_AluminiumHoneycomb", 86.5 * kg/m3, 2); //Prepend subdetector name // density taken from na48 documentation.
    LKrAluminiumHoneycombFillerMaterial->AddMaterial(G4Material::GetMaterial("G4_Galactic"), 89.0 *perCent);  //check percentage
    LKrAluminiumHoneycombFillerMaterial->AddMaterial(G4Material::GetMaterial("G4_Al"), 11 *perCent); 

 
}

void LKrMaterialParameters::SetMaterialProperties()
{
    // Use here the material property tables prepared
}

TObjArray LKrMaterialParameters::GetHashTable()
{
    TObjArray LKrMaterialParameters;
    std::ostringstream Buffer;
    TString Value;
    TObjArray ParameterData;

    Buffer << fMaterialPropertiesNEntries;
    Value = Buffer.str();
    Buffer.str("");
    Float_t MaterialPropertiesNEntries = fMaterialPropertiesNEntries;
    ParameterData.Add(new TVector(1, &MaterialPropertiesNEntries));
    LKrMaterialParameters.Add(new DetectorParameter("fMaterialPropertiesNEntries",Value.Data(),
                "Material Properties N Entries", ParameterData));
    ParameterData.Clear();


    return LKrMaterialParameters;
}

void LKrMaterialParameters::Print(){
    G4cout << "fMaterialPropertiesNEntries= "<< fMaterialPropertiesNEntries << G4endl;
}
