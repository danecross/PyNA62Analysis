// LAVMaterialParameters.cc
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// 2009-02-16 - Emanuele Leonardi (Emanuele.Leonardi@roma1.infn.it)
//   - First implementation of LAV materials
// 2011-01-24 Domenico Di Filippo (difilippo@na.infn.it)
//   - Added Optical surface
//   - Enable LAV_PbGl_SF57 optical properties
//
// --------------------------------------------------------------
#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"

#include "TVector.h"

#include "LAVMaterialParameters.hh"
#include "LAVGeometryParameters.hh"
#include "DetectorParameter.hh"

int LAVOpticSize = 2;
double LAVOpticEnergies[] = {1.88*eV, 3.1*eV};
double LAVOpticOne[] = {1., 1.};
double LAVOpticZero[] = {0., 0.};

LAVMaterialParameters* LAVMaterialParameters::fInstance = 0;

LAVMaterialParameters::LAVMaterialParameters() {
  // Prepare here every Material Property Table
  DefineMaterials();
  SetMaterialProperties();
  DefineSurfaces();
}

LAVMaterialParameters::~LAVMaterialParameters(){}

LAVMaterialParameters* LAVMaterialParameters::GetInstance() {
  if ( fInstance == 0 ) { fInstance = new LAVMaterialParameters(); }
  return fInstance;
}

void LAVMaterialParameters::DefineMaterials() {

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
  nistMgr->FindOrBuildMaterial("G4_POLYVINYLIDENE_FLUORIDE");

  // Components for Schott SF57 Leadglass

  // SiO2
  std::vector<G4String> SiO2El(2);
  std::vector<G4int> SiO2NA(2);
  SiO2El[0] = "Si"; SiO2NA[0] = 1;
  SiO2El[1] = "O";  SiO2NA[1] = 2;
  nistMgr->ConstructNewMaterial("LAV_SiO2",SiO2El,SiO2NA,2.32,true);

  // PbO
  std::vector<G4String> PbOEl(2);
  std::vector<G4int> PbONA(2);
  PbOEl[0] = "Pb"; PbONA[0] = 1;
  PbOEl[1] = "O";  PbONA[1] = 1;
  nistMgr->ConstructNewMaterial("LAV_PbO",PbOEl,PbONA,9.53,true);

  // Na2O
  std::vector<G4String> Na2OEl(2);
  std::vector<G4int> Na2ONA(2);
  Na2OEl[0] = "Na"; Na2ONA[0] = 2;
  Na2OEl[1] = "O";  Na2ONA[1] = 1;
  nistMgr->ConstructNewMaterial("LAV_Na2O",Na2OEl,Na2ONA,2.27,true);

  // K2O
  std::vector<G4String> K2OEl(2);
  std::vector<G4int> K2ONA(2);
  K2OEl[0] = "K"; K2ONA[0] = 2;
  K2OEl[1] = "O"; K2ONA[1] = 1;
  nistMgr->ConstructNewMaterial("LAV_K2O",K2OEl,K2ONA,2.32,true);

  // Lead Glass (PbGl)
  G4Material* PbGl = new G4Material("LAV_PbGl_SF57",5.57*g/cm3,4);
  PbGl->AddMaterial(G4Material::GetMaterial("LAV_SiO2"), 24.0*perCent); // 22-26%
  PbGl->AddMaterial(G4Material::GetMaterial("LAV_PbO"),  74.0*perCent); // 72-76%
  PbGl->AddMaterial(G4Material::GetMaterial("LAV_Na2O"),  1.0*perCent); // 0.5-1.5%
  PbGl->AddMaterial(G4Material::GetMaterial("LAV_K2O"),   1.0*perCent); // 0.5-1.5%

  // Add optical properties to leadglass

  // Number of bins for Lead Glass properties table
  const G4int nEntries = 10;

  G4double PhotonEnergy[nEntries] =
    { 1.88914*eV, 1.92582*eV, 1.95929*eV, 2.10392*eV, 2.11001*eV,
      2.27035*eV, 2.55059*eV, 2.58300*eV, 2.84497*eV, 3.06360*eV};

  G4double RefractiveIndexPbGl[nEntries] =
    { 1.83650,    1.83808,    1.83957,    1.84636,    1.84666,
      1.85504,    1.87204,    1.87425,    1.89393,    1.91366 };

  // Abosrption lengths as declared by provider
  //G4double AbsorptionPbGl[nEntries] =
  //  {  11.*cm,     34.*cm,    270.*cm,    850.*cm,    830.*cm,    730.*cm,
  //    610.*cm,    420.*cm,    420.*cm,    420.*cm,    420.*cm,    420.*cm,
  //    420.*cm,    144.*cm,    130.*cm,     34.*cm,      8.*cm };

  // As 2007 experimental measurement does not show a significant variation
  // of absorption length from that declared by provider, we use the values
  // from data sheet for SF57. WARNING: error on these numbers may be high!
  G4double AbsorptionPbGl[nEntries] =
    {   420.*cm,    420.*cm,    420.*cm,    420.*cm,    420.*cm,
        420.*cm,    144.*cm,    130.*cm,     34.*cm,      8.*cm };

  // Lead Glass does not scintillate
  ////G4double ScintilFast[nEntries] =
  ////          { 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00 };
  ////G4double ScintilSlow[nEntries] =
  ////          { 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00 };

  // Define property table for Lead Glass
  G4MaterialPropertiesTable* myMPTPbGl = new G4MaterialPropertiesTable();
  myMPTPbGl->AddProperty("RINDEX",PhotonEnergy, RefractiveIndexPbGl, nEntries);
  myMPTPbGl->AddProperty("ABSLENGTH",PhotonEnergy, AbsorptionPbGl, nEntries);

  ////myMPTPbGl->AddProperty("FASTCOMPONENT",PhotonEnergy, ScintilFast, nEntries);
  ////myMPTPbGl->AddProperty("SLOWCOMPONENT",PhotonEnergy, ScintilSlow, nEntries);
  ////myMPTPbGl->AddConstProperty("SCINTILLATIONYIELD",0./MeV);
  ////myMPTPbGl->AddConstProperty("RESOLUTIONSCALE",0.);
  ////myMPTPbGl->AddConstProperty("FASTTIMECONSTANT", 0.*ns);
  ////myMPTPbGl->AddConstProperty("SLOWTIMECONSTANT",0.*ns);
  ////myMPTPbGl->AddConstProperty("YIELDRATIO",0.);

  PbGl->SetMaterialPropertiesTable(myMPTPbGl);

  // block glue
  G4Material *glue = new G4Material("LAV_GLUE",5.57*g/cm3,1);
  glue->AddMaterial(G4Material::GetMaterial("LAV_SiO2"), 100*perCent);
  G4double RefractiveIndexGLUE[nEntries] =
    { 1.56,    1.56,    1.56,    1.56,    1.56,
      1.56,    1.56,    1.56,    1.56,    1.56 };
  G4MaterialPropertiesTable* gluetable = new G4MaterialPropertiesTable();
  gluetable->AddProperty("RINDEX", PhotonEnergy, RefractiveIndexGLUE, 10);
  glue->SetMaterialPropertiesTable(gluetable);

  // tyvek
  nistMgr->FindOrBuildMaterial("G4_POLYETHYLENE");
  G4Material* LAVTyvek = new G4Material("LAV_Tyvek",G4Material::GetMaterial("G4_POLYETHYLENE")->GetDensity(),1);
  LAVTyvek->AddMaterial(G4Material::GetMaterial("G4_POLYETHYLENE"), 100.0*perCent);
  G4MaterialPropertiesTable* TyvekProperties = new G4MaterialPropertiesTable();

  const unsigned int TyvRefSiz = 84;
  double TyvRefEne[TyvRefSiz] = {
     1.79304112241, 1.81453612088, 1.83655273711, 1.8591101914, 1.8822286601,
     1.90592933578, 1.93023449203, 1.95516755328, 1.98075317012, 2.00701730055,
     2.03398729787, 2.06169200568, 2.09016186076, 2.11942900467, 2.14952740473,
     2.18049298559, 2.21236377223, 2.24518004574, 2.27898451318, 2.313822493,
     2.34974211772, 2.38679455585, 2.42503425501, 2.46451920895, 2.50531125103,
     2.54747637728, 2.59108510271, 2.63621285486, 2.68294040916, 2.73135437143,
     2.78154771363, 2.83362036974, 2.88767989984, 2.94384223177, 3.00223249091,
     3.06298593086, 3.11018948116, 3.12624897934, 3.14247518512, 3.15887070782,
     3.17543821154, 3.19218041652, 3.20910010071, 3.22620010124, 3.24348331607,
     3.26095270557, 3.27861129422, 3.29646217241, 3.31450849818, 3.33275349908,
     3.35120047417, 3.36985279592, 3.38871391232, 3.40778734897, 3.42707671132,
     3.44658568691, 3.46631804771, 3.4862776526, 3.50646844981, 3.52689447962,
     3.54755987695, 3.56846887426, 3.58962580434, 3.61103510338, 3.632701314,
     3.65462908853, 3.6768231923, 3.69928850713, 3.72203003483, 3.74505290103,
     3.76836235892, 3.79196379332, 3.81586272479, 3.84006481395, 3.86457586596,
     3.88940183512, 3.91454882974, 3.94002311714, 3.96583112882, 3.99197946594,
     4.01847490487, 4.04532440312, 4.07253510538, 4.10011434989};
  double TyvRef[TyvRefSiz] = {
     0.821996590825, 0.818785666642, 0.821917537488, 0.821917537488, 0.821917537488,
     0.821917537488, 0.821917537488, 0.823522845179, 0.82512815287, 0.821917537488,
     0.82512815287, 0.82512815287, 0.825048790731, 0.825048790731, 0.828099755429,
     0.824810704316, 0.826335877863, 0.827860742608, 0.827860742608, 0.827701400727,
     0.827621729786, 0.830829565948, 0.827542058846, 0.829867801823, 0.827462387905,
     0.829065997184, 0.830669606462, 0.827462387905, 0.827462387905, 0.830669606462,
     0.827462387905, 0.827462387905, 0.821047950789, 0.821047950789, 0.821047950789,
     0.811426295116, 0.79363373601, 0.763171026952, 0.743716494973, 0.718071098595,
     0.685882185825, 0.653768620766, 0.637621976828, 0.631122464734, 0.615015193063,
     0.605347217076, 0.630848711677, 0.638731193952, 0.656277020183, 0.646610588207,
     0.644885619705, 0.655960498036, 0.652697695101, 0.633439561254, 0.642912991922,
     0.64918476247, 0.68106564515, 0.676171440746, 0.679368468835, 0.680802545765,
     0.671051656414, 0.682169921441, 0.66619404506, 0.658015267176, 0.661145594011,
     0.65629122508, 0.646585266434, 0.644926381581, 0.635286815386, 0.630345750266,
     0.622216149114, 0.609452740927, 0.615715247413, 0.606027199289, 0.609098853726,
     0.602720855259, 0.605733837299, 0.602429037279, 0.603964246893, 0.607092566516,
     0.602253946491, 0.584671372811, 0.562149818424, 0.535025568813};

  TyvekProperties->AddProperty("RINDEX", LAVOpticEnergies,LAVOpticOne,LAVOpticSize);
  TyvekProperties->AddProperty("REFLECTIVITY", TyvRefEne,TyvRef,TyvRefSiz);
  LAVTyvek->SetMaterialPropertiesTable(TyvekProperties);

  // Vacuum
  G4Material* LAVGalactic = new G4Material("LAV_Galactic",G4Material::GetMaterial("G4_Galactic")->GetDensity(),1);
  LAVGalactic->AddMaterial(G4Material::GetMaterial("G4_Galactic"), 100.0*perCent);
  G4MaterialPropertiesTable* VacuumProperties = new G4MaterialPropertiesTable();
  G4double ones[nEntries] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
  VacuumProperties->AddProperty("RINDEX", PhotonEnergy, ones, 10);
  LAVGalactic->SetMaterialPropertiesTable(VacuumProperties);

}

void LAVMaterialParameters::SetMaterialProperties() {
  // Use here the material property tables prepared
}

void LAVMaterialParameters::DefineSurfaces(){

   G4MaterialPropertiesTable *mattable;
   G4double OpticalEnergyRange[2]={1.87*eV,3.1*eV};
   G4double ZeroProperty[2]={0.0,0.0};

   // Used for the tyvek wrapping of the blocks
   double tyvref[]={0.9,0.9};
   mattable = new G4MaterialPropertiesTable();
   mattable->AddProperty("REFLECTIVITY", LAVOpticEnergies,tyvref,LAVOpticSize);
   mattable->AddProperty("EFFICIENCY", LAVOpticEnergies,LAVOpticZero,LAVOpticSize);
   mattable->AddProperty("SPECULARLOBECONSTANT", LAVOpticEnergies,LAVOpticZero,LAVOpticSize);
   mattable->AddProperty("SPECULARSPIKECONSTANT", LAVOpticEnergies,LAVOpticZero,LAVOpticSize);
   mattable->AddProperty("BACKSCATTERCONSTANT", LAVOpticEnergies,LAVOpticZero,LAVOpticSize);
   fDiffusiveSurface =
      new G4OpticalSurface("Diffusive",unified,groundfrontpainted,dielectric_dielectric);
   fDiffusiveSurface->SetMaterialPropertiesTable(mattable);

   // Used for the metal plate of the blocks
   mattable = new G4MaterialPropertiesTable();
   double badref[]={0.5,0.5};
   mattable->AddProperty("REFLECTIVITY", LAVOpticEnergies,badref,LAVOpticSize);
   mattable->AddProperty("EFFICIENCY", LAVOpticEnergies,LAVOpticZero,LAVOpticSize);
   fBadReflactiveSurface =
      new G4OpticalSurface("BadReflective",unified,polished,dielectric_metal);
   fBadReflactiveSurface->SetMaterialPropertiesTable(mattable);

   // A material that absorb all optical photons
   // Used in the photomultiplier
   mattable = new G4MaterialPropertiesTable();
   mattable->AddProperty("REFLECTIVITY",OpticalEnergyRange,ZeroProperty,2);
   mattable->AddProperty("EFFICIENCY",OpticalEnergyRange,ZeroProperty,2);
   fPhotonKillerSurface =
     new G4OpticalSurface("PhotonKiller",unified,polished,dielectric_metal);
   fPhotonKillerSurface->SetMaterialPropertiesTable(mattable);
}

TObjArray LAVMaterialParameters::GetHashTable() {
  TObjArray LAVMaterialParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;
  Buffer << fMaterialPropertiesNEntries;
  Value = Buffer.str();
  Buffer.str("");
  Float_t MaterialPropertiesNEntries = fMaterialPropertiesNEntries;
  ParameterData.Add(new TVector(1, &MaterialPropertiesNEntries));
  LAVMaterialParameters.Add(new DetectorParameter("fMaterialPropertiesNEntries",Value.Data(),
					       "Material Properties N Entries", ParameterData));
  ParameterData.Clear();
  return LAVMaterialParameters;
}

void LAVMaterialParameters::Print(){
  G4cout << "fMaterialPropertiesNEntries= "<< fMaterialPropertiesNEntries << G4endl;
}
