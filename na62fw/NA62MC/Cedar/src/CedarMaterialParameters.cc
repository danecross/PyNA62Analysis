// --------------------------------------------------------------
// History:
//
// 2012-12-21 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - parameters are read from a configuration file
//
// 2012-10-26 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - the measured QW transmittances (different for each QW)
//
// 2012-08-16 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - improved transmittance parameterizations
//
// 2012-06-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - optical cap lens transmittance
//
// 2011-11-18 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - switching between West/North, various gas options
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - absorption, reflectivity, PMT quantum efficiency
//
// 2011-06-10 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - optical properties of H2, N2, quartz
// - optical surfaces
//
// 2009-11-16 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Cedar hydrogen parameters (P, T, density) added
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------

/// \class CedarMaterialParameters
/// \Brief
/// Properties of CEDAR materials and optical surfaces
/// \EndBrief

#include "TVector.h"
#include "TString.h"
#include "TRegexp.h"
#include "TObjString.h"

#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"
#include "CedarGeometryParameters.hh"
#include "CedarMaterialParameters.hh"
#include "DetectorParameter.hh"
#include "NA62ConditionsService.hh"

CedarMaterialParameters* CedarMaterialParameters::fInstance = 0;

CedarMaterialParameters::CedarMaterialParameters() :
  fOpticalPropertiesEnabled(true),
  fRadiatorHydrogenPressure(-99.99),
  fRadiatorNitrogenPressure(-99.99)
{
  ParseConfFile("Cedar-MCSettings.dat");
  DefineMaterials();
  DefineProperties();
  BuildPropertyTables();
  AssignOpticalProperties();
}

CedarMaterialParameters::~CedarMaterialParameters() {}

void CedarMaterialParameters::ParseConfFile (TString ConfFileName) {
  TString Line;
  NA62ConditionsService::GetInstance()->Open(ConfFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(ConfFileName))) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("NitrogenPressure")) {
      fRadiatorNitrogenPressure = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * bar;
      continue;
    }
    else if (Line.BeginsWith("HydrogenPressure")) {
      fRadiatorHydrogenPressure = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * bar;
      continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(ConfFileName);
}

CedarMaterialParameters* CedarMaterialParameters::GetInstance() {
  if (!fInstance) fInstance = new CedarMaterialParameters();
  return fInstance;
}

void CedarMaterialParameters::DefineMaterials() {

  // Geant4 STP: P=1atm=101.325kPa, T=273.15K
  // NB: 1bar=100kPa

  fRadiatorHydrogenTemperature = 293.15*kelvin;
  fRadiatorNitrogenTemperature = 293.15*kelvin;
  fNitrogenTemperature         = 293.15*kelvin;

  // Radiator pressures are tuned to optimize light collection at the diaphragm
  if (fRadiatorNitrogenPressure<0) fRadiatorNitrogenPressure = 1.710*bar;
  if (fRadiatorHydrogenPressure<0) fRadiatorHydrogenPressure = 3.670*bar;
  G4cout <<"[CedarMaterialParameters] N2/H2 pressures used [bar]: " <<
    fRadiatorNitrogenPressure/bar << " " << fRadiatorHydrogenPressure/bar << G4endl;

  // Nitrogen enclosure pressure: Geneva average (defined at sea level)
  // corrected for the elevation of the experimental hall (445m)
  fNitrogenPressure = 1.021*bar - 1.2*kg/m3 * 445*m * 9.81*m/s/s;

  // The gas constant
  Double_t R = 8.314472*joule/kelvin/mole;

  fRadiatorHydrogenDensity =
    (2 *  1.0079*g/mole) / R * fRadiatorHydrogenPressure / fRadiatorHydrogenTemperature;
  fRadiatorNitrogenDensity =
    (2 * 14.0067*g/mole) / R * fRadiatorNitrogenPressure / fRadiatorNitrogenTemperature;
  fNitrogenDensity         =
    (2 * 14.0067*g/mole) / R * fNitrogenPressure / fNitrogenTemperature;

  // Standard materials
  G4NistManager* nistMgr = G4NistManager::Instance();
  nistMgr->FindOrBuildMaterial("G4_AIR");
  nistMgr->FindOrBuildMaterial("G4_Galactic");
  nistMgr->FindOrBuildMaterial("G4_He");
  nistMgr->FindOrBuildMaterial("G4_H");
  nistMgr->FindOrBuildMaterial("G4_N");
  nistMgr->FindOrBuildMaterial("G4_Ne");
  nistMgr->FindOrBuildMaterial("G4_Fe");
  nistMgr->FindOrBuildMaterial("G4_Cr");
  nistMgr->FindOrBuildMaterial("G4_Ni");
  nistMgr->FindOrBuildMaterial("G4_Si");
  nistMgr->FindOrBuildMaterial("G4_Al");
  nistMgr->FindOrBuildMaterial("G4_SILICON_DIOXIDE");

  // Radiator: pressurized hydrogen
  G4Material *CedarRadiatorHydrogen = new
    G4Material("Cedar_RadiatorHydrogen", fRadiatorHydrogenDensity, 1, kStateGas);
  CedarRadiatorHydrogen->AddMaterial(G4Material::GetMaterial("G4_H"), 1.0);

  // Radiator: pressurized nitrogen (test runs 2006, 2011, 2012)
  G4Material *CedarRadiatorNitrogen = new
    G4Material("Cedar_RadiatorNitrogen", fRadiatorNitrogenDensity, 1, kStateGas);
  CedarRadiatorNitrogen->AddMaterial(G4Material::GetMaterial("G4_N"), 1.0);

  // Nitrogen
  G4Material *CedarNitrogen = new
    G4Material("Cedar_Nitrogen", fNitrogenDensity, 1, kStateGas);
  CedarNitrogen->AddMaterial(G4Material::GetMaterial("G4_N"), 1.0);

  // Quartz
  G4Material *CedarQuartz = new
    G4Material("Cedar_Quartz",
	       G4Material::GetMaterial("G4_SILICON_DIOXIDE")->GetDensity(),
	       1, kStateSolid);
  CedarQuartz->AddMaterial(G4Material::GetMaterial("G4_SILICON_DIOXIDE"), 1.0);

  // Non-absorptive quartz (for PMT windows)
  G4Material *CedarIdealQuartz = new
    G4Material("Cedar_IdealQuartz",
	       G4Material::GetMaterial("G4_SILICON_DIOXIDE")->GetDensity(),
	       1, kStateSolid);
  CedarIdealQuartz->
    AddMaterial(G4Material::GetMaterial("G4_SILICON_DIOXIDE"), 1.0);

  // External lens quartz (absorption properties supplied by Edmund Optics)
  G4Material *CedarExternalLensQuartz = new
    G4Material("Cedar_ExternalLensQuartz",
	       G4Material::GetMaterial("G4_SILICON_DIOXIDE")->GetDensity(),
	       1, kStateSolid);
  CedarExternalLensQuartz->
    AddMaterial(G4Material::GetMaterial("G4_SILICON_DIOXIDE"), 1.0);

  // Quartz window quartz (absorption properties measured in July/August 2012)
  G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();
  for (int iWindow=0; iWindow<fNSectors; iWindow++) {
    G4String fMaterialName = Form("Cedar_QuartzWindowQuartz%d", iWindow+1);
    G4Material *CedarQuartzWindowQuartz = new
      G4Material(fMaterialName,
		 G4Material::GetMaterial("G4_SILICON_DIOXIDE")->GetDensity(),
		 1, kStateSolid);
    CedarQuartzWindowQuartz->
      AddMaterial(G4Material::GetMaterial("G4_SILICON_DIOXIDE"), 1.0);
  }

  // Stainless steel
  G4Material* StainlessSteel = new
    G4Material("Cedar_StainlessSteel",7.88*g/cm3,4);
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Fe"), 71.5*perCent);
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Cr"), 18.0*perCent);
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Ni"), 10.0*perCent);
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Si"),  0.5*perCent);

  // Extensive output about materials definition
  // nistMgr->SetVerbose(1);
  // nistMgr->PrintElement("all");
  // nistMgr->ListMaterials("all");
}

void CedarMaterialParameters::DefineProperties() {

  G4int fNSectors               = CedarGeometryParameters::GetInstance()->GetNSectors();
  fMaterialPropertiesNEntries   = 50;
  fPhotonEnergy                 = new G4double[fMaterialPropertiesNEntries];
  fWaveLength                   = new G4double[fMaterialPropertiesNEntries];

  fRadiatorHydrogenRefIndex           = new G4double[fMaterialPropertiesNEntries];
  fRadiatorNitrogenRefIndex           = new G4double[fMaterialPropertiesNEntries];
  fNitrogenRefIndex                   = new G4double[fMaterialPropertiesNEntries];
  fQuartzRefIndex                     = new G4double[fMaterialPropertiesNEntries];
  fQuartzAbsorptionLength             = new G4double[fMaterialPropertiesNEntries];
  fExternalLensQuartzAbsorptionLength = new G4double[fMaterialPropertiesNEntries];
  fQuartzWindowQuartzAbsorptionLength = new G4double[fNSectors*fMaterialPropertiesNEntries];

  fManginMirrorReflectivity     = new G4double[fMaterialPropertiesNEntries];
  fManginMirrorEfficiency       = new G4double[fMaterialPropertiesNEntries];
  fManginMirrorSpecularLobe     = new G4double[fMaterialPropertiesNEntries];
  fManginMirrorSpecularSpike    = new G4double[fMaterialPropertiesNEntries];

  fSphericalMirrorReflectivity  = new G4double[fMaterialPropertiesNEntries];
  fSphericalMirrorEfficiency    = new G4double[fMaterialPropertiesNEntries];
  fSphericalMirrorSpecularLobe  = new G4double[fMaterialPropertiesNEntries];
  fSphericalMirrorSpecularSpike = new G4double[fMaterialPropertiesNEntries];

  fLightGuideReflectivity       = new G4double[fMaterialPropertiesNEntries];
  fLightGuideEfficiency         = new G4double[fMaterialPropertiesNEntries];
  fLightGuideSpecularLobe       = new G4double[fMaterialPropertiesNEntries];
  fLightGuideSpecularSpike      = new G4double[fMaterialPropertiesNEntries];

  fLensTransmittance            = new G4double[fMaterialPropertiesNEntries];

  // limits for the Cherenkov photon spectrum

  fCherenkovLambdaMin       = 180*nm;
  fCherenkovLambdaMax       = 700*nm;
  fCherenkovPhotonEnergyMin = ELConvert(fCherenkovLambdaMax);
  fCherenkovPhotonEnergyMax = ELConvert(fCherenkovLambdaMin);

  G4int iLGType = CedarGeometryParameters::GetInstance()->GetLightGuideType();

  // define the optical properties

  for (G4int i=0; i<fMaterialPropertiesNEntries; i++) {

    fPhotonEnergy[i] = fCherenkovPhotonEnergyMin +
      (fCherenkovPhotonEnergyMax - fCherenkovPhotonEnergyMin) / (fMaterialPropertiesNEntries - 1) * i;
    fWaveLength[i]   = ELConvert(fPhotonEnergy[i]);

    // Refractive indices
    fRadiatorHydrogenRefIndex[i] =
      H2RefIndex(fWaveLength[i], fRadiatorHydrogenPressure/STP_Pressure,
		 fRadiatorHydrogenTemperature/STP_Temperature);

    fRadiatorNitrogenRefIndex[i] =
      N2RefIndex(fWaveLength[i], fRadiatorNitrogenPressure/STP_Pressure,
		 fRadiatorNitrogenTemperature/STP_Temperature);

    fNitrogenRefIndex[i] =
      N2RefIndex(fWaveLength[i], fNitrogenPressure/STP_Pressure,
		 fNitrogenTemperature/STP_Temperature);

    fQuartzRefIndex[i] = QuartzRefIndex(fWaveLength[i]);

    // Absorption lengths
    fQuartzAbsorptionLength[i]             = QuartzAbsorptionLength(fWaveLength[i]);
    fExternalLensQuartzAbsorptionLength[i] = ExternalLensAbsorptionLength(fWaveLength[i]);

    for (G4int iWin=0; iWin<fNSectors; iWin++) {
      fQuartzWindowQuartzAbsorptionLength [iWin*fMaterialPropertiesNEntries + i] =
	QuartzWindowAbsorptionLength(iWin+1, fWaveLength[i]);
      if (iLGType==3) { // UV optical filters attached to windows in 2011: identical transmittances
	fQuartzWindowQuartzAbsorptionLength [iWin*fMaterialPropertiesNEntries + i] =
	  QuartzWindowAndFilterAbsorptionLength(fWaveLength[i]);
      }
    }

    // Optical properties of the Mangin mirror
    fManginMirrorReflectivity[i]  = ManginMirrorReflectivity(fWaveLength[i]);
    fManginMirrorSpecularSpike[i] = 1.00;
    fManginMirrorSpecularLobe[i]  = 0.00;
    fManginMirrorEfficiency[i]    = 0.00;

    // Optical properties of the spherical mirrors (2012, 2014).
    // Reflectivity measured by Thomas Schneider.
    // Reflectivities of mirrors and LG mylar cone inserts are similar.

    fSphericalMirrorReflectivity[i]  = SphericalMirrorAndConeReflectivity(fWaveLength[i]);
    fSphericalMirrorSpecularSpike[i] = 1.00;
    fSphericalMirrorSpecularLobe[i]  = 0.00;
    fSphericalMirrorEfficiency[i]    = 0.00;

    // Optical properties of the light guides.
    // Test beam 2011: reflectivity measured by Bozydar to be 75% at lambda=600nm.
    // 2012+ configurations: reflectivity measured by Thomas Schneider.
    // Reflectivities of mirrors and LG mylar cone inserts are similar: the same function is used.

    fLightGuideReflectivity[i]  = SphericalMirrorAndConeReflectivity(fWaveLength[i]);
    if (iLGType==3) fLightGuideReflectivity[i]  = 0.75;
    fLightGuideSpecularSpike[i] = 1.00;
    fLightGuideSpecularLobe[i]  = 0.00;
    fLightGuideEfficiency[i]    = 0.00;

    // Optical properties of lens boundaries
    // Photons that are not transmitted will be reflected
    fLensTransmittance[i]       = 1.00;
  }
}

void CedarMaterialParameters::BuildPropertyTables() {

  //////////////////////
  // Materials        //
  //////////////////////

  fRadiatorHydrogenMPT = new G4MaterialPropertiesTable();
  fRadiatorHydrogenMPT->AddProperty("RINDEX", fPhotonEnergy,
				    fRadiatorHydrogenRefIndex,
				    fMaterialPropertiesNEntries);

  fRadiatorNitrogenMPT = new G4MaterialPropertiesTable();
  fRadiatorNitrogenMPT->AddProperty("RINDEX", fPhotonEnergy,
				    fRadiatorNitrogenRefIndex,
				    fMaterialPropertiesNEntries);

  fNitrogenMPT = new G4MaterialPropertiesTable();
  fNitrogenMPT->AddProperty("RINDEX", fPhotonEnergy, fNitrogenRefIndex,
                            fMaterialPropertiesNEntries);

  fQuartzMPT = new G4MaterialPropertiesTable();
  fQuartzMPT->AddProperty("RINDEX", fPhotonEnergy, fQuartzRefIndex,
			  fMaterialPropertiesNEntries);
  fQuartzMPT->AddProperty("ABSLENGTH", fPhotonEnergy, fQuartzAbsorptionLength,
			  fMaterialPropertiesNEntries);

  fIdealQuartzMPT = new G4MaterialPropertiesTable();
  fIdealQuartzMPT->AddProperty("RINDEX", fPhotonEnergy, fQuartzRefIndex,
			       fMaterialPropertiesNEntries);

  fExternalLensQuartzMPT = new G4MaterialPropertiesTable();
  fExternalLensQuartzMPT->AddProperty("RINDEX", fPhotonEnergy, fQuartzRefIndex,
			  fMaterialPropertiesNEntries);

  G4int iLGType = CedarGeometryParameters::GetInstance()->GetLightGuideType();

  if (iLGType==48 || iLGType==64) {
    // Absorption length for the 2014 external lenses: coated lenses are used.
    fExternalLensQuartzMPT->AddProperty
      ("ABSLENGTH", fPhotonEnergy, fExternalLensQuartzAbsorptionLength, fMaterialPropertiesNEntries);
  }
  else {
    // In 2012 (32-PMT configuration), non-coated lenses were used.
    // Standard quartz is assumed, finite absorption length guarantees that a photon is not "trapped" in a lens.
    fExternalLensQuartzMPT->AddProperty
      ("ABSLENGTH", fPhotonEnergy, fQuartzAbsorptionLength, fMaterialPropertiesNEntries);
  }

  // Refractive index & absorption of the quarts windows

  G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();
  for (int iWindow=0; iWindow<fNSectors; iWindow++) {
    fQuartzWindowQuartzMPT[iWindow] = new G4MaterialPropertiesTable();
    fQuartzWindowQuartzMPT[iWindow]->AddProperty
      ("RINDEX", fPhotonEnergy, fQuartzRefIndex, fMaterialPropertiesNEntries);
    fQuartzWindowQuartzMPT[iWindow]->AddProperty
      ("ABSLENGTH", fPhotonEnergy,
       &fQuartzWindowQuartzAbsorptionLength[iWindow*fMaterialPropertiesNEntries],
       fMaterialPropertiesNEntries);
  }

  //////////////////////
  // Optical surfaces //
  //////////////////////

  // Mangin mirror

  fManginMirrorOpticalSurfacePT = new G4MaterialPropertiesTable();
  fManginMirrorOpticalSurfacePT->
    AddProperty("REFLECTIVITY", fPhotonEnergy,
		fManginMirrorReflectivity, fMaterialPropertiesNEntries);
  fManginMirrorOpticalSurfacePT->
    AddProperty("EFFICIENCY", fPhotonEnergy,
		fManginMirrorEfficiency, fMaterialPropertiesNEntries);
  fManginMirrorOpticalSurfacePT->
    AddProperty("SPECULARSPIKECONSTANT", fPhotonEnergy,
		fManginMirrorSpecularSpike, fMaterialPropertiesNEntries);
  fManginMirrorOpticalSurfacePT->
    AddProperty("SPECULARLOBECONSTANT", fPhotonEnergy,
		fManginMirrorSpecularLobe, fMaterialPropertiesNEntries);

  // Spherical mirror

  fSphericalMirrorOpticalSurfacePT = new G4MaterialPropertiesTable();
  fSphericalMirrorOpticalSurfacePT->
    AddProperty("REFLECTIVITY", fPhotonEnergy,
                fSphericalMirrorReflectivity, fMaterialPropertiesNEntries);
  fSphericalMirrorOpticalSurfacePT->
    AddProperty("EFFICIENCY", fPhotonEnergy,
                fSphericalMirrorEfficiency, fMaterialPropertiesNEntries);
  fSphericalMirrorOpticalSurfacePT->
    AddProperty("SPECULARSPIKECONSTANT", fPhotonEnergy,
                fSphericalMirrorSpecularSpike, fMaterialPropertiesNEntries);
  fSphericalMirrorOpticalSurfacePT->
    AddProperty("SPECULARLOBECONSTANT", fPhotonEnergy,
                fSphericalMirrorSpecularLobe, fMaterialPropertiesNEntries);

  // Light guides

  fLightGuideOpticalSurfacePT = new G4MaterialPropertiesTable();
  fLightGuideOpticalSurfacePT->
    AddProperty("REFLECTIVITY", fPhotonEnergy,
                fLightGuideReflectivity, fMaterialPropertiesNEntries);
  fLightGuideOpticalSurfacePT->
    AddProperty("EFFICIENCY", fPhotonEnergy,
                fLightGuideEfficiency, fMaterialPropertiesNEntries);
  fLightGuideOpticalSurfacePT->
    AddProperty("SPECULARSPIKECONSTANT", fPhotonEnergy,
                fLightGuideSpecularSpike, fMaterialPropertiesNEntries);
  fLightGuideOpticalSurfacePT->
    AddProperty("SPECULARLOBECONSTANT", fPhotonEnergy,
                fLightGuideSpecularLobe, fMaterialPropertiesNEntries);

  // Lenses and quartz windows: no loss at boundaries

  fLensOpticalSurfacePT = new G4MaterialPropertiesTable();
  fLensOpticalSurfacePT->
    AddProperty("TRANSMITTANCE", fPhotonEnergy,
		fLensTransmittance, fMaterialPropertiesNEntries);
}

//////////////////////////////
// Enable the Cherenkov effect

void CedarMaterialParameters::AssignOpticalProperties() {

  fOpticalPropertiesEnabled = true;

  G4Material *CedarRadiatorHydrogen =
    G4Material::GetMaterial("Cedar_RadiatorHydrogen");
  CedarRadiatorHydrogen->SetMaterialPropertiesTable(fRadiatorHydrogenMPT);

  G4Material *CedarRadiatorNitrogen =
    G4Material::GetMaterial("Cedar_RadiatorNitrogen");
  CedarRadiatorNitrogen->SetMaterialPropertiesTable(fRadiatorNitrogenMPT);

  G4Material *CedarNitrogen = G4Material::GetMaterial("Cedar_Nitrogen");
  CedarNitrogen->SetMaterialPropertiesTable(fNitrogenMPT);

  G4Material *CedarQuartz = G4Material::GetMaterial("Cedar_Quartz");
  CedarQuartz->SetMaterialPropertiesTable(fQuartzMPT);

  G4Material *CedarIdealQuartz = G4Material::GetMaterial("Cedar_IdealQuartz");
  CedarIdealQuartz->SetMaterialPropertiesTable(fIdealQuartzMPT);

  G4Material *CedarExternalLensQuartz = G4Material::GetMaterial("Cedar_ExternalLensQuartz");
  CedarExternalLensQuartz->SetMaterialPropertiesTable(fExternalLensQuartzMPT);

  G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();
  for (int iWindow=0; iWindow<fNSectors; iWindow++) {
    G4String name = Form("Cedar_QuartzWindowQuartz%d", iWindow+1);
    G4Material *CedarQuartzWindowQuartz = G4Material::GetMaterial(name);
    CedarQuartzWindowQuartz->SetMaterialPropertiesTable(fQuartzWindowQuartzMPT[iWindow]);
  }
}

///////////////////////////////
// Disable the Cherenkov effect

void CedarMaterialParameters::RemoveOpticalProperties() {

  fOpticalPropertiesEnabled = false;

  G4Material *CedarRadiatorHydrogen =
    G4Material::GetMaterial("Cedar_RadiatorHydrogen");
  CedarRadiatorHydrogen->SetMaterialPropertiesTable(0);

  G4Material *CedarRadiatorNitrogen =
    G4Material::GetMaterial("Cedar_RadiatorNitrogen");
  CedarRadiatorNitrogen->SetMaterialPropertiesTable(0);

  G4Material *CedarNitrogen = G4Material::GetMaterial("Cedar_Nitrogen");
  CedarNitrogen->SetMaterialPropertiesTable(0);

  G4Material *CedarQuartz = G4Material::GetMaterial("Cedar_Quartz");
  CedarQuartz->SetMaterialPropertiesTable(0);

  G4Material *CedarIdealQuartz = G4Material::GetMaterial("Cedar_IdealQuartz");
  CedarIdealQuartz->SetMaterialPropertiesTable(0);

  G4Material *CedarExternalLensQuartz = G4Material::GetMaterial("Cedar_ExternalLensQuartz");
  CedarExternalLensQuartz->SetMaterialPropertiesTable(0);

  G4int fNSectors = CedarGeometryParameters::GetInstance()->GetNSectors();
  for (int iWindow=0; iWindow<fNSectors; iWindow++) {
    G4String name = Form("Cedar_QuartzWindowQuartz%d", iWindow+1);
    G4Material *CedarQuartzWindowQuartz = G4Material::GetMaterial(name);
    CedarQuartzWindowQuartz->SetMaterialPropertiesTable(0);
  }
}

///////////////////////////////////////////////////////////////////////////
// Auxiliary functions

/*** Refractive indices ***/

// Sellmeier formula for fused silica
// https://refractiveindex.info/?shelf=glass&book=fused_silica&page=Malitson
G4double CedarMaterialParameters::QuartzRefIndex(G4double wavelength) {
  G4double x = wavelength/um;
  G4double A = 0.6961663*x*x/(x*x-0.0684043*0.0684043);
  G4double B = 0.4079426*x*x/(x*x-0.1162414*0.1162414);
  G4double C = 0.8974794*x*x/(x*x-9.8961610*9.8961610);
  G4double n = sqrt(1.0+A+B+C);
  return n;
}

// Lau's parameterization for fused silica
G4double CedarMaterialParameters::QuartzRefIndex_Lau(G4double wavelength) {
  G4double x = wavelength/angstrom;
  G4double an0 = 457044.55181;
  G4double a1  = 653024.33502;
  G4double sl  = 990.42659;
  return 1 + 1e-6 * (an0 + a1 * exp(-x/sl));
}

// Evgueni's parameterization for H2
// T=273 K, P=101325 Pa
G4double CedarMaterialParameters::H2RefIndex
(G4double wavelength, G4double PressureFactor, G4double TemperatureFactor) {
  G4double A  = 138.744;
  G4double B  = 185.655;
  G4double L1 = 103.564*nm;
  G4double L2 = 13073.1*um;
  G4double ReducedIndex =
    A + B * exp(-wavelength/L1-pow(wavelength/L2,2.0));
  ReducedIndex   *= (PressureFactor/TemperatureFactor);
  G4double Index  = 1 + (ReducedIndex * 1e-6);
  return Index;
}

// Lau's parameterization for H2
G4double CedarMaterialParameters::H2RefIndex_Lau
(G4double wavelength, G4double PressureFactor, G4double TemperatureFactor) {
  G4double A = 139.36439;
  G4double B = 221.41569;
  G4double L = 951.95887*angstrom;
  G4double ReducedIndex = A + B * exp(-wavelength/L);
  ReducedIndex         *= (PressureFactor/TemperatureFactor);
  G4double Index        = 1 + (ReducedIndex * 1e-6);
  return Index;
}

// Nitrogen: parametrization from http://refractiveindex.info
// T=273K
G4double CedarMaterialParameters::N2RefIndex
(G4double wavelength, G4double PressureFactor, G4double TemperatureFactor) {
  G4double x            = wavelength/um;
  G4double ReducedIndex = 68.5520E-6 + 32431.57E-6*x*x/(144*x*x-1);
  ReducedIndex         *= (PressureFactor/TemperatureFactor);
  G4double Index        = 1 + ReducedIndex;
  return Index;
}

// Neon: parametrization from http://refractiveindex.info
// T=273K
G4double CedarMaterialParameters::NeRefIndex
(G4double wavelength, G4double PressureFactor, G4double TemperatureFactor) {
  G4double x            = wavelength/um;
  G4double ReducedIndex = 0.012055 * (0.1063*x*x/(184.661*x*x-1) + 1.8290*x*x/(376.840*x*x-1));
  ReducedIndex         *= (PressureFactor/TemperatureFactor);
  G4double Index        = 1 + ReducedIndex;
  return Index;
}

// Helium: parametrization from http://refractiveindex.info
// T=273K
G4double CedarMaterialParameters::HeRefIndex
(G4double wavelength, G4double PressureFactor, G4double TemperatureFactor) {
  G4double x            = wavelength/um;
  G4double ReducedIndex = 0.01470091/(423.98-1.0/x/x);
  ReducedIndex         *= (PressureFactor/TemperatureFactor);
  G4double Index        = 1 + ReducedIndex;
  return Index;
}

/*** Transmittances of QW and external lenses ***/

// External (optical cap) lens transmittance specified by Edmund Optics
// Parameterized by John Fry, 24 April 2012

G4double CedarMaterialParameters::ExternalLensTransmittance(G4double Wavelength) {
  G4double x = Wavelength/nm;
  if (x<180)           return 0;
  if (x>=180 && x<225) return 0.500+0.01800*(x-200);
  if (x>=225 && x<235) return 0.950+0.00450*(x-225);
  if (x>=235 && x<420) return 0.995;
  if (x>=420 && x<450) return 0.995-0.00050*(x-420);
  if (x>=450 && x<650) return 0.980-0.00075*(x-450);
  if (x>=650 && x<750) return 0.830-0.00030*(x-650);
  if (x>=750)          return 0.80;
  return 0;
}

// Quartz windows (including effects of reflections).
// Measured by Thomas Schneider, parameterized by Evgueni Goudzovski, Sep 2012.

G4double CedarMaterialParameters::QuartzWindowTransmittance(G4int iWindow, G4double Wavelength) {
  G4double x = Wavelength/nm;
  G4double f = -999;

  if (iWindow==1 || iWindow==3 || iWindow==7) {
    if      (x>370) f = 0.900 + 0.00011    * (x-370);
    else if (x>270) f = 0.910 - 0.010/100. * (x-270);
    else if (x>260) f = 0.890 + 0.020/ 10. * (x-260);
    else if (x>250) f = 0.830 + 0.060/ 10. * (x-250);
    else if (x>240) f = 0.790 + 0.040/ 10. * (x-240);
    else if (x>230) f = 0.790;
    else if (x>225) f = 0.740 + 0.050/  5. * (x-225);
    else if (x>220) f = 0.640 + 0.100/  5. * (x-220);
    else if (x>210) f = 0.350 + 0.290/ 10. * (x-210);
    else if (x>205) f = 0.260 + 0.090/  5. * (x-205);
    else            f = 0.235 + 0.025/  5. * (x-200);
  }

  else if (iWindow==2 || iWindow==4 || iWindow==8) {
    f = 0.9 + 0.00008*(x-200);
  }

  else if (iWindow==5) {
    if (x>380)      f = 0.92 + 0.0001*(x-380);
    else if (x>360) f = 0.92;
    else if (x>250) f = 0.95 - 0.030 / 110. * (x-250);
    else if (x>240) f = 0.95;
    else if (x>210) f = 0.925 + 0.025 / 30. * (x-210);
    else            f = 0.900 + 0.025 / 10. * (x-200);
  }

  else if (iWindow==6) {
    if      (x>500) f = 0.946;
    else if (x>300) f = 0.950 - 0.00002*(x-300);
    else if (x>240) f = 0.958 - 0.008 / 60. * (x-240);
    else if (x>210) f = 0.940 + 0.018 / 30. * (x-210);
    else            f = 0.914 + 0.026 / 10. * (x-200);
  }

  if (f<0.001) f = 0.001;
  if (f>0.999) f = 0.999;
  return f;
}

// Quartz windows with attached UV filters (including effects of reflections).
// Used in 2011.
// Measured by Thomas Schneider, parameterized by Evgueni Goudzovski, July 2012.

G4double CedarMaterialParameters::QuartzWindowAndFilterTransmittance(G4double wavelength) {
  G4double x = wavelength/nm;
  G4double f;
  if      (x>350) f = 0.905 + 0.00008*(x-350);
  else if (x>300) f = 0.875 + 0.030*(x-300)/50.0;
  else if (x>280) f = 0.845 + 0.030*(x-280)/20.0;
  else if (x>270) f = 0.815 + 0.030*(x-270)/10.0;
  else if (x>260) f = 0.760 + 0.055*(x-260)/10.0;
  else if (x>250) f = 0.670 + 0.090*(x-250)/10.0;
  else if (x>240) f = 0.490 + 0.180*(x-240)/10.0;
  else            f = 0.232 + 0.258*(x-230)/10.0;
  if (f<0.001)    f = 0.001;
  if (f>0.999)    f = 0.999;
  return f;
}

/*** Absorption lengths ***/

// Quartz: from the Cedar manual (CERN 82-13), p.13
G4double CedarMaterialParameters::QuartzAbsorptionLength(G4double wavelength) {
  G4double x = wavelength/nm;
  G4double Transmittance_12cm;
  if      (x>400) Transmittance_12cm = 0.999;
  else if (x>220) Transmittance_12cm = 0.900 + 0.099/(400-220)*(x-220);
  else if (x>180) Transmittance_12cm = 0.700 + 0.200/(220-200)*(x-200);
  else            Transmittance_12cm = 0.500; // non-physical but fool-proof
  G4double AbsLength = - 12*cm / log(Transmittance_12cm);
  return AbsLength;
}

// The external lens.
// Assumption: Transmittance is fully due to absorption,
// and is specified at the centre of the lens (where the lens is the thickest).

G4double CedarMaterialParameters::ExternalLensAbsorptionLength(G4double wavelength) {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  G4double AbsLength =
    - GeoPars->GetExternalLensMaxThickness() / log(ExternalLensTransmittance(wavelength));
  return AbsLength;
}

///////////////////////////////////////////////////////////////////
// Quartz windows. Model: Transmittance is fully due to absorption.

// Without the UV filters: differs between the windows
G4double CedarMaterialParameters::QuartzWindowAbsorptionLength(G4int iWindow, G4double Wavelength) {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  G4double AbsLength =
    - GeoPars->GetQuartzWindowZLength() / log(QuartzWindowTransmittance(iWindow, Wavelength));
  return AbsLength;
}

// Quartz window + UV filter: the same for all windows

G4double CedarMaterialParameters::QuartzWindowAndFilterAbsorptionLength(G4double Wavelength) {
  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();
  G4double AbsLength =
    - GeoPars->GetQuartzWindowZLength() / log(QuartzWindowAndFilterTransmittance(Wavelength));
  return AbsLength;
}

/*** Reflectivities ***/

// Losses due to reflections in all lenses are parametrized the single
// Mangin mirror reflectivity, following the Cedar manual (CERN 82-13), p.13

G4double CedarMaterialParameters::ManginMirrorReflectivity(G4double wavelength) {
  G4double x1   = wavelength/nm;
  G4double x    = (x1<630) ? x1 : 630;
  G4double refl =
    -0.261299
    +0.00938637*x
    -2.47446e-05*x*x
    +2.31118e-08*x*x*x
    -5.79119e-12*x*x*x*x;
  if (refl<0) refl = 0;
  if (refl>1) refl = 1;
  return refl;
}

// 2012+ spherical mirrors and LG cones: reflectivity measured by Thomas Schneider.

G4double CedarMaterialParameters::SphericalMirrorAndConeReflectivity(G4double wavelength) {
  G4double x    = wavelength/nm;
  G4double refl = (x>450) ? 0.90 : 0.90 - 0.05/(450-200)*(450-x);
  return refl;
}

/*** Other functions ***/

// Convert photon energy to wavelength and vice versa.
// The conversion constant is approximately 1240 eV*nm.
G4double CedarMaterialParameters::ELConvert (G4double value) {
  return 1.986446e-25 * joule * m / value;
}

//////////////////////////////////////////////////////////////////////////

TObjArray CedarMaterialParameters::GetHashTable() {
  TObjArray CedarMaterialParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fMaterialPropertiesNEntries;
  Value = Buffer.str();
  Buffer.str("");
  Float_t MaterialPropertiesNEntries = fMaterialPropertiesNEntries;
  ParameterData.Add(new TVector(1, &MaterialPropertiesNEntries));
  CedarMaterialParameters.Add
    (new DetectorParameter
     ("fMaterialPropertiesNEntries",Value.Data(),
      "Material Properties N Entries", ParameterData));
  ParameterData.Clear();

  return CedarMaterialParameters;
}

void CedarMaterialParameters::Print() {
  G4cout << "fMaterialPropertiesNEntries= "<< fMaterialPropertiesNEntries << G4endl;
}
