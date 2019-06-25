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

#include "MUV1MaterialParameters.hh"
#include "MUV1GeometryParameters.hh"
#include "DetectorParameter.hh"







MUV1MaterialParameters* MUV1MaterialParameters::fInstance = 0;

MUV1MaterialParameters::MUV1MaterialParameters() :
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

MUV1MaterialParameters::~MUV1MaterialParameters(){}

MUV1MaterialParameters* MUV1MaterialParameters::GetInstance()
{
  if ( fInstance == 0 )
    fInstance = new MUV1MaterialParameters();
  return fInstance;
}

void MUV1MaterialParameters::DefineMaterials()
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
  nistMgr->FindOrBuildMaterial("G4_GLASS_PLATE");

  G4String symbol;             //a=mass of a mole;
  G4double a, z, density;      //z=mean number of protons;  

  G4int ncomponents;
  G4double fractionmass;



  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //+                             Scintillator                                +
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //Define POPOP
  std::vector<G4String> POPOPEl(4);
  std::vector<G4int> POPOPNA(4);
  POPOPEl[0] = "C"; POPOPNA[0] = 24;
  POPOPEl[1] = "O"; POPOPNA[1] = 2;
  POPOPEl[2] = "H"; POPOPNA[2] = 16;
  POPOPEl[3] = "N"; POPOPNA[3] = 2;
  nistMgr->ConstructNewMaterial("POPOP",POPOPEl,POPOPNA,1.204,true);

  // Define p-Terphenyl
  std::vector<G4String> pTerphenylEl(2);
  std::vector<G4int> pTerphenylNA(2);
  pTerphenylEl[0] = "C"; pTerphenylNA[0] = 18;
  pTerphenylEl[1] = "H"; pTerphenylNA[1] = 14;
  nistMgr->ConstructNewMaterial("pTerphenyl",pTerphenylEl,pTerphenylNA,1.23,true);

  //  Define Scintillator material
  G4Material* Scint = new G4Material("Scint",1.06*g/cm3,3);
  Scint->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 97.7*perCent); // 98%
  Scint->AddMaterial(G4Material::GetMaterial("POPOP"),  0.3*perCent); // 0.3%
  Scint->AddMaterial(G4Material::GetMaterial("pTerphenyl"),  2.0*perCent); // 2%

  // Add optical properties to Scint



  // Number of bins for Scintillator material properties table
  const G4int nEntriesRef = 2;
  const G4int nEntriesEmmission = 11;

  // Emission aus doi:10.1016/j.nima.2007.04.147
  //G4double EmissionScint[nEntries]={1.0, 5.0, 5.0, 10.0, 10.0};


  G4double PhotonEnergyRef[nEntriesRef] =
            { 1.0*eV,  3.5*eV};

  G4double RefractiveIndexScint[nEntriesRef] =
               { 1.5, 1.5 };


  G4double AbsorptionScint[nEntriesRef] =
         {0.44*m,  0.44*m};

  G4double PhotonEnergy[nEntriesEmmission] =
                { 2.42*eV,2.52*eV,2.75*eV,2.81*eV,2.95*eV,3.02*eV,3.08*eV,3.13*eV,3.26*eV,3.31*eV,3.38*eV};

     	 	 //	 {0.5*m,  0.5*m,  0.5*m, 0.5*m, 0.5*m};
  G4double ScintilFast[nEntriesEmmission] =
            { 0,0.1,0.3,0.7,0.55,1.0,0.7,0.5,0.9,0.5,0};

  // Define property table for Scint
  G4MaterialPropertiesTable* myMPTScint = new G4MaterialPropertiesTable();
  myMPTScint->AddProperty("RINDEX",PhotonEnergyRef, RefractiveIndexScint, nEntriesRef);
  myMPTScint->AddProperty("ABSLENGTH",PhotonEnergyRef, AbsorptionScint, nEntriesRef);
  //myMPTScint->AddProperty("COMPONENT",PhotonEnergy, EmissionScint, nEntries);
  //myMPTScint->AddConstProperty("TIMECONSTANT",0.5*ns);


  myMPTScint->AddProperty("FASTCOMPONENT",PhotonEnergy, ScintilFast,nEntriesEmmission);
  //myMPTScint->AddProperty("SLOWCOMPONENT",PhotonEnergy, ScintilSlow, nEntries);
  myMPTScint->AddConstProperty("SCINTILLATIONYIELD",1500/MeV); // 15% of 10 000 -> quantum efficiency
  myMPTScint->AddConstProperty("RESOLUTIONSCALE",1.0);
  myMPTScint->AddConstProperty("FASTTIMECONSTANT", 3.3*ns);
  //myMPTScint->AddConstProperty("SLOWTIMECONSTANT",10.*ns);
  myMPTScint->AddConstProperty("YIELDRATIO",1.0);


  Scint->SetMaterialPropertiesTable(myMPTScint);


  Scint->GetIonisation()->SetBirksConstant(0.151*mm/MeV);  //Using Number from CALICE





  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //+                       Scintillator dummy                                +
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // For fast simulation - scintillation not needed
  //  Define Scintillator material
  G4Material* NonScint = new G4Material("NonScint",1.06*g/cm3,3);
  NonScint->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 97.7*perCent); // 98%
  NonScint->AddMaterial(G4Material::GetMaterial("POPOP"),  0.3*perCent); // 0.3%
  NonScint->AddMaterial(G4Material::GetMaterial("pTerphenyl"),  2.0*perCent); // 2%
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //+                             AIR                                         +
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


   G4Element* N  = new G4Element("Nitrogen",symbol="N" , z= 7., a= 14.01*g/mole);
   G4Element* O  = new G4Element("Oxygen"  ,symbol="O" , z= 8., a= 16.00*g/mole);

   G4Material* Air =
   new G4Material("Air"  , density= 1.290*mg/cm3, ncomponents=2);
   Air->AddElement(N, fractionmass=0.7);
   Air->AddElement(O, fractionmass=0.3);


  // Add optical properties to Air

  G4MaterialPropertiesTable* myMPTAir = new G4MaterialPropertiesTable();

  G4double RefractiveIndexAir[nEntriesRef] =
                 { 1.0,  1.0 };

  myMPTAir->AddProperty("RINDEX",PhotonEnergyRef, RefractiveIndexAir, nEntriesRef);
  Air->SetMaterialPropertiesTable(myMPTAir);

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //+                          FIBER MATERIAL                                 +
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  //Define fiber material
  G4Material* WLSMatCore = new G4Material("WLSMatCore", 1.05*g/cm3,1);
  WLSMatCore->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 100*perCent);


  // Add optical properties to fiber
  const G4int NUMENTRIES1 = 2;

  G4double PhotonEnergy_WLS_REFINDEX_core[NUMENTRIES1]={
		  3.5*eV, 2.0*eV
  };

  //Data: http://www.detectors.saint-gobain.com/fibers.aspx
  G4double WLS_REFINDEX_core[NUMENTRIES1]={
  		  1.6, 1.6
    };

  G4double WLS_ABSORB_core[NUMENTRIES1] =
                {7*m,  7*m};

  // Data from "Simulation of optical processes in GEANT4"
  // http://www-zeuthen.desy.de/lcdet/Feb_05_WS/talks/rd_lcdet_sim.pdf

  const G4int NUMENTRIES2 = 24;
  //Data for Bcf91a
  G4double PhotonEnergy_WLS_EM_core[NUMENTRIES2] ={
		  3.5*eV, 2.67*eV, 2.66*eV, 2.64*eV, 2.63*eV,2.61*eV,
		  2.58*eV, 2.56*eV, 2.55*eV, 2.53*eV, 2.50*eV, 2.48*eV,
		  2.46*eV, 2.45*eV, 2.44*eV,2.43*eV, 2.41*eV, 2.37*eV,
		  2.33*eV, 2.25*eV,2.24*eV, 2.19*eV, 2.15*eV, 2.0*eV };

   //Data for Bcf91a
   G4double WLS_EMISSION_core[NUMENTRIES2] ={
 		  0, 0.02, 0.09, 0.20, 0.29,0.40, 0.59, 0.70, 0.80,
 		  0.89,1.00, 0.96, 0.88, 0.79, 0.69,0.59, 0.50, 0.40,
 		  0.31, 0.22,0.19, 0.10, 0.06, 0};


   const G4int NUMENTRIES3 = 42;


  //Data for Bcf91a
   G4double PhotonEnergy_WLS_ABS_core[NUMENTRIES3] ={
 		  3.5*eV, 3.477*eV, 3.340*eV, 3.321*eV, 3.291*eV,
 		  3.214*eV, 3.162*eV, 3.129*eV, 3.091*eV, 3.086*eV,
 		  3.049*eV, 3.008*eV, 2.982*eV, 2.958*eV, 2.928*eV,
 		  2.905*eV, 2.895*eV, 2.890*eV, 2.858*eV, 2.813*eV,
 		  2.774*eV, 2.765*eV, 2.752*eV, 2.748*eV, 2.739*eV,
 		  2.735*eV, 2.731*eV, 2.723*eV, 2.719*eV, 2.698*eV,
 		  2.674*eV, 2.626*eV, 2.610*eV, 2.583*eV, 2.556*eV,
 		  2.530*eV, 2.505*eV, 2.480*eV, 2.455*eV, 2.431*eV,
 		  2.407*eV, 2.0*eV };

  //Data for Bcf91a

   G4double WLS_ABSLENGTH_core[NUMENTRIES3] ={
   0.28*cm, 0.28*cm, 0.26*cm, 0.2*cm, 0.16*cm, 0.12*cm,
   0.08*cm, 0.06*cm, 0.02*cm, 0.02*cm, 0.06*cm, 0.08*cm,
   0.1*cm, 0.12*cm, 0.12*cm, 0.16*cm, 0.16*cm, 0.18*cm,
   0.18*cm, 0.2*cm, 0.21*cm, 0.22*cm, 0.22*cm, 0.23*cm,
   0.23*cm, 0.24*cm, 0.25*cm, 0.25*cm, 0.26*cm, 0.27*cm,
   0.3*cm, 2.69*cm, 3.49*cm, 3.99*cm, 5*cm, 11.6*cm, 21.6*cm,
   33.1*cm, 175*cm, 393*cm, 617*cm,794*cm };



    G4MaterialPropertiesTable* MPTFiber = new G4MaterialPropertiesTable();

    MPTFiber->AddProperty("RINDEX",PhotonEnergy_WLS_REFINDEX_core,WLS_REFINDEX_core,NUMENTRIES1);
    MPTFiber->AddProperty("ABSLENGTH",PhotonEnergy_WLS_REFINDEX_core, WLS_ABSORB_core, NUMENTRIES1);
    MPTFiber->AddProperty("WLSCOMPONENT",PhotonEnergy_WLS_EM_core,WLS_EMISSION_core,NUMENTRIES2);
    MPTFiber->AddProperty("WLSABSLENGTH",PhotonEnergy_WLS_ABS_core,WLS_ABSLENGTH_core,NUMENTRIES3);
    MPTFiber->AddConstProperty("WLSTIMECONSTANT", 2.7*ns); // http://www.detectors.saint-gobain.com/uploadedFiles/SGdetectors/Documents/Brochures/Organics-Brochure.pdf


    //MPTFiber->AddConstProperty("WLSTIMECONSTANT", 0.5*ns);

    WLSMatCore->SetMaterialPropertiesTable(MPTFiber);


    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //+                         DUMMY FIBER MATERIAL                            +
    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //Define dummy fiber material
      G4Material* DummyWLSMatCore = new G4Material("DummyWLSMatCore", 1.05*g/cm3,1);
      DummyWLSMatCore->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 100*perCent);


    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //+                     FIBER CLADDING                                      +
    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    //Define ACRYLIC -> Wikipedia: Acrylonitrile  C3H3N   0.81 g/cm3
    std::vector<G4String> ACRYLICEl(3);
    std::vector<G4int> ACRYLICNA(3);
    ACRYLICEl[0] = "C"; ACRYLICNA[0] = 3;
    ACRYLICEl[1] = "H"; ACRYLICNA[1] = 3;
    ACRYLICEl[2] = "N"; ACRYLICNA[2] = 1;
    nistMgr->ConstructNewMaterial("ACRYLIC",ACRYLICEl,ACRYLICNA,0.81,true);





    G4Material* FiberCladding = new G4Material("FiberCladding",1.05*g/cm3,1);
    FiberCladding->AddMaterial(G4Material::GetMaterial("ACRYLIC"), 100*perCent);


      G4MaterialPropertiesTable* myMPTCladding = new G4MaterialPropertiesTable();

       G4double RINDEX_cladding[NUMENTRIES1] =
                      { 1.49,1.49 };

       myMPTCladding->AddProperty("RINDEX",PhotonEnergy_WLS_REFINDEX_core, RINDEX_cladding, NUMENTRIES1);
       myMPTCladding->AddProperty("ABSLENGTH",PhotonEnergy_WLS_REFINDEX_core, WLS_ABSORB_core, NUMENTRIES1);

       FiberCladding->SetMaterialPropertiesTable(myMPTCladding);


       //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       //+                     FIBER CLADDING2                                     +
       //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      G4Material* FiberCladding2 = new G4Material("FiberCladding2",1.05*g/cm3,1);
      FiberCladding2->AddMaterial(G4Material::GetMaterial("ACRYLIC"), 100*perCent);


      G4MaterialPropertiesTable* myMPTCladding2 = new G4MaterialPropertiesTable();

      G4double RINDEX_cladding2[NUMENTRIES1] =
                       { 1.42,1.42 };

      myMPTCladding2->AddProperty("RINDEX",PhotonEnergy_WLS_REFINDEX_core, RINDEX_cladding2, NUMENTRIES1);
      myMPTCladding2->AddProperty("ABSLENGTH",PhotonEnergy_WLS_REFINDEX_core, WLS_ABSORB_core, NUMENTRIES1);


      FiberCladding2->SetMaterialPropertiesTable(myMPTCladding);


       //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       //+                     OPTICAL CEMENT                                      +
       //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


       G4Material* OpticalCement = new G4Material("OpticalCement",1.05*g/cm3,1);
       OpticalCement->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 100*perCent);


       G4MaterialPropertiesTable* myMPTOpticalCement = new G4MaterialPropertiesTable();

       G4double RINDEX_OpticalCement[NUMENTRIES1] =
                         { 1.59,1.59 };

       myMPTOpticalCement->AddProperty("RINDEX",PhotonEnergy_WLS_REFINDEX_core, RINDEX_OpticalCement, NUMENTRIES1);
       myMPTOpticalCement->AddProperty("ABSLENGTH",PhotonEnergy_WLS_REFINDEX_core, WLS_ABSORB_core, NUMENTRIES1);


       OpticalCement->SetMaterialPropertiesTable(myMPTOpticalCement);





 // Materials to be specific for this subdetector
  // Example: Stainless Steel (StainlessSteel)
  G4Material* StainlessSteel = new G4Material("MUV1_StainlessSteel",7.88*g/cm3,4); //Prepend subdetector name
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Fe"), 71.5*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Cr"), 18.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Ni"), 10.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Si"),  0.5*perCent); 

 
 //nistMgr->RegisterMaterial(StainlessSteel);

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //+                     GLASS                                               +
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  G4Material* Glass = new G4Material("Glass",2.400*g/cm3,1);
  Glass->AddMaterial(G4Material::GetMaterial("G4_GLASS_PLATE"), 100*perCent );



  G4MaterialPropertiesTable* myGlass = new G4MaterialPropertiesTable();

  G4double RINDEX_Glass[NUMENTRIES1] =
                    { 1.46,1.46 };

  myGlass->AddProperty("RINDEX",PhotonEnergy_WLS_REFINDEX_core, RINDEX_Glass, NUMENTRIES1);

  Glass->SetMaterialPropertiesTable(myGlass);

/*
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   //+                     VISUALISATION                                       +
   //+      Change Material in order to use GDML visualisation                 +
   //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   G4Material* Scint = new G4Material("Scint",1.06*g/cm3,3);
   Scint->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 100*perCent);
   G4Material* NonScint = new G4Material("NonScint",1.06*g/cm3,3);
   NonScint->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 100*perCent);
   G4Material* Air = new G4Material("Air",1.06*g/cm3,3);
   Air->AddMaterial(G4Material::GetMaterial("G4_AIR"), 100*perCent);
   G4Material* WLSMatCore = new G4Material("WLSMatCore",1.06*g/cm3,3);
   WLSMatCore->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 100*perCent);
   G4Material* DummyWLSMatCore = new G4Material("DummyWLSMatCore",1.06*g/cm3,3);
   DummyWLSMatCore->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 100*perCent);
   G4Material* FiberCladding = new G4Material("FiberCladding",1.06*g/cm3,3);
   FiberCladding->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 100*perCent);
   G4Material* FiberCladding2 = new G4Material("FiberCladding2",1.06*g/cm3,3);
   FiberCladding2->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 100*perCent);
   G4Material* OpticalCement = new G4Material("OpticalCement",1.06*g/cm3,3);
   OpticalCement->AddMaterial(G4Material::GetMaterial("G4_POLYSTYRENE"), 100*perCent);
   G4Material* StainlessSteel = new G4Material("MUV1_StainlessSteel",1.06*g/cm3,3);
   StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Fe"), 100*perCent);
   G4Material* Glass = new G4Material("Glass",1.06*g/cm3,3);
   Glass->AddMaterial(G4Material::GetMaterial("G4_GLASS_PLATE"), 100*perCent);
*/
}

void MUV1MaterialParameters::SetMaterialProperties()
{
  // Use here the material property tables prepared

}

TObjArray MUV1MaterialParameters::GetHashTable()
{
  TObjArray MUV1MaterialParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fMaterialPropertiesNEntries;
  Value = Buffer.str();
  Buffer.str("");
  Float_t MaterialPropertiesNEntries = fMaterialPropertiesNEntries;

  ParameterData.Add(new TVector(1, &MaterialPropertiesNEntries));
  MUV1MaterialParameters.Add(new DetectorParameter("fMaterialPropertiesNEntries",Value.Data(),
						   "Material Properties N Entries", ParameterData));
  ParameterData.Clear();

  return MUV1MaterialParameters;
}

void MUV1MaterialParameters::Print()
{
  G4cout << "fMaterialPropertiesNEntries= "<< fMaterialPropertiesNEntries << G4endl;
}
