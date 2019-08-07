#include "G4Material.hh"
#include "G4NistManager.hh"
#include "G4MaterialPropertiesTable.hh"

#include "TVector.h"
#include "TObjString.h"

#include "RICHMaterialParameters.hh"
#include "RICHGeometryParameters.hh"
#include "DetectorParameter.hh"

#ifndef RICHFR
#define RICHFR 0.70
#endif
#ifndef RICHFSp
#define RICHFSp 0.
#endif
#ifndef RICHFL
#define RICHFL 0.75
#endif
#ifndef RICHFS
#define RICHFS 0.04
#endif
#ifndef RICHTR
#define RICHTR 0.70
#endif
#ifndef RICHTSp
#define RICHTSp 0.
#endif
#ifndef RICHTL
#define RICHTL 0.75
#endif
#ifndef RICHTS
#define RICHTS 0.070
#endif

#ifndef RICHMS
//#define RICHMS 0.0055
#define RICHMS 0.0000055
#endif
#ifndef RICHML
//#define RICHML 0.0055
#define RICHML 0.0000055
#endif


RICHMaterialParameters* RICHMaterialParameters::fInstance = 0;

RICHMaterialParameters::RICHMaterialParameters()
{
  // Add optical properties to Neon

  // Number of bins for Neon properties table
#ifdef RICHNEWOPT 
  fMaterialPropertiesNEntries=50;
#else
  fMaterialPropertiesNEntries=17;
#endif

  fPhotonEnergy = new G4double[fMaterialPropertiesNEntries];
  fNeonScintilFast = new G4double[fMaterialPropertiesNEntries];
  fNeonAbsorptionLength = new G4double[fMaterialPropertiesNEntries];
  fNeonRefIndex = new G4double[fMaterialPropertiesNEntries];

  fQuartzRefIndex = new G4double[fMaterialPropertiesNEntries];
  fQuartzAbsorptionLength = new G4double[fMaterialPropertiesNEntries];

  fVacuumRefIndex = new G4double[fMaterialPropertiesNEntries];

  fMirrorReflectivity = new G4double[fMaterialPropertiesNEntries];
  fMirrorEfficiency = new G4double[fMaterialPropertiesNEntries];

  fConeReflectivity = new G4double[fMaterialPropertiesNEntries];
  fConeEfficiency = new G4double[fMaterialPropertiesNEntries];

  fPMTsWindowReflectivity = new G4double[fMaterialPropertiesNEntries];
  fPMTsWindowEfficiency = new G4double[fMaterialPropertiesNEntries];
  fPMTsWindowSpecularSpike = new G4double[fMaterialPropertiesNEntries];
  fPMTsWindowSpecularLobe = new G4double[fMaterialPropertiesNEntries];
  fPMTsWindowBackscatter = new G4double[fMaterialPropertiesNEntries];

  fVesselReflectivity = new G4double[fMaterialPropertiesNEntries];
  fVesselEfficiency = new G4double[fMaterialPropertiesNEntries];
  fVesselSpecularSpike = new G4double[fMaterialPropertiesNEntries];
  fVesselSpecularLobe = new G4double[fMaterialPropertiesNEntries];
  fVesselBackscatter = new G4double[fMaterialPropertiesNEntries];

  fMinWavelength=160e-9*m;
  fNeonPressure = 0.984*bar*1.012;//1.011;
  fNeonTemperature = STP_Temperature+24*kelvin;
#ifndef RICHNEWOPT
  G4double Zero[19];
  G4double PhotonEnergy[19]={ 2.00*eV, 2.06*eV, 2.17*eV, 2.29*eV, 2.48*eV, 2.69*eV, 2.88*eV, 
			      3.09*eV, 3.35*eV, 3.87*eV, 4.13*eV, 4.42*eV, 4.76*eV, 4.95*eV, 
			      6.19*eV, 6.51*eV, 6.88*eV, 7.29*eV, 7.75*eV};
  G4double QuartzTransmission[19]= { 0.935, 0.935, 0.935, 0.930, 0.930, 0.925, 0.925,
				     0.925, 0.920, 0.920, 0.915, 0.91, 0.90, 0.89,
				     0.8, 0.5, 0.0, 0.0, 0.0};
  G4double PMQuantumResponseU03[19]= { 0.2, 0.9, 2.2, 4.6, 12.4, 16.2, 18.7, 
				      19.2, 20.1, 19.4, 18.6, 17.7, 14.3, 12.4,
				      6.2, 2.6, 0.0, 0.0, 0.0};
  /*
  G4double PMQuantumResponseU06[19]= { 0.2, 0.9, 2.2, 4.6, 12.4, 16.2, 18.7, 
				      19.2, 20.1, 19.4, 18.6, 17.7, 14.3, 12.4,
				      11.8, 11.7, 10.3, 7.3, 4.6};
  */
  G4double AlMgF2GlassReflectivity[19]= { 0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 
					  0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.86,
					  0.84, 0.85, 0.84, 0.82, 0.81};
  G4double MirrorSpecularSpike[19];
  G4double MirrorSpecularLobe[19];
  G4double AlMgF2MylarReflectivity[19]= { 0.90, 0.90, 0.90, 0.91, 0.91, 0.90, 0.90, 
					  0.89, 0.89, 0.87, 0.87, 0.86, 0.86, 0.86,
					  0.78, 0.77, 0.76, 0.76, 0.73};
#else
  G4double * Zero = new G4double[fMaterialPropertiesNEntries];
  G4double * QuartzTransmission = new G4double[fMaterialPropertiesNEntries];
  G4double * PMQuantumResponseU03 = new G4double[fMaterialPropertiesNEntries];
  G4double * PMQuantumResponseU06 = new G4double[fMaterialPropertiesNEntries];
  G4double * AlMgF2GlassReflectivity = new G4double[fMaterialPropertiesNEntries];
  G4double * AlMgF2MylarReflectivity = new G4double[fMaterialPropertiesNEntries];

  G4double * MirrorSpecularSpike = new G4double[fMaterialPropertiesNEntries];
  G4double * MirrorSpecularLobe = new G4double[fMaterialPropertiesNEntries];
#endif
  G4double MeanNeonRefIndex=0,NeonPhotonNumber=0,
    TrackPathLengthInNeon=RICHGeometryParameters::GetInstance()->GetMirrorFlangeDistance();

  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Zero[i] = 0;
#ifdef RICHNEWOPT 
    fPhotonEnergy[i]=2.00*eV + (7.75 - 2.00)*eV / (fMaterialPropertiesNEntries - 1)* i;
#else
    fPhotonEnergy[i]=PhotonEnergy[i];
#endif
    fNeonScintilFast[i]=1.0;
    fNeonAbsorptionLength[i]=3000*m;
    fNeonRefIndex[i]=1.+2.61303e27/(39160e27-
				    (fPhotonEnergy[i]/h_Planck)*
				    (fPhotonEnergy[i]/h_Planck)*(s*s))*
      (fNeonPressure/(fNeonTemperature))/(STP_Pressure/(STP_Temperature));
    fQuartzRefIndex[i]=1.44+359/(-5320+42.8*299792458*1e9/(fPhotonEnergy[i]/h_Planck*s));
#ifdef RICHNEWOPT
    G4double NormalReflection = (fQuartzRefIndex[i] - fNeonRefIndex[i])/(fQuartzRefIndex[i] + fNeonRefIndex[i])*(fQuartzRefIndex[i] - fNeonRefIndex[i])/(fQuartzRefIndex[i] + fNeonRefIndex[i]);
    G4cout <<  " RICHNEWOPT used " << G4endl;
    //QuartzTransmission[i] = 0.01 * (90. - 2e6*exp(-0.065 * 1240./(fPhotonEnergy[i]/eV)));
    QuartzTransmission[i] = 0.01 * (9.13392e+01 - 1.24518e+13 * exp(-1.68759e-01 * 1240./(fPhotonEnergy[i]/eV)) 
		    - 1e5 * exp(-4.8e-2 * 1240./(fPhotonEnergy[i]/eV)));
    QuartzTransmission[i] = (QuartzTransmission[i] > 0 ? QuartzTransmission[i] : 0.);
    AlMgF2GlassReflectivity[i] = 0.01 * (85. - 11500. * exp( -0.035 * 1240./(fPhotonEnergy[i]/eV)));
    AlMgF2GlassReflectivity[i] = (AlMgF2GlassReflectivity[i] > 0 ? AlMgF2GlassReflectivity[i] : 0.);
    AlMgF2MylarReflectivity[i] = 0.01 * (-143.897 + 3.73798 * 1240./(fPhotonEnergy[i]/eV)
            - 0.0247806 * 1240./(fPhotonEnergy[i]/eV) * 1240./(fPhotonEnergy[i]/eV)  
            + 8.4999e-05 * pow(1240./(fPhotonEnergy[i]/eV),3) 
            - 1.5824e-07 * pow(1240./(fPhotonEnergy[i]/eV),4)  
            + 1.51945e-10 * pow(1240./(fPhotonEnergy[i]/eV),5) 
            - 5.90409e-14 * pow(1240./(fPhotonEnergy[i]/eV),6));
    AlMgF2MylarReflectivity[i] = (AlMgF2MylarReflectivity[i] > 0 ? AlMgF2MylarReflectivity[i] : 0.);
    PMQuantumResponseU03 [i] = -7408.01 + 184.084 * 1240./(fPhotonEnergy[i]/eV)
        - 1.98272 * 1240./(fPhotonEnergy[i]/eV) * 1240./(fPhotonEnergy[i]/eV)    
        + 0.0121346   * pow(1240./(fPhotonEnergy[i]/eV),3)    
        - 4.64744e-05 * pow(1240./(fPhotonEnergy[i]/eV),4)  
        + 1.15586e-07 * pow(1240./(fPhotonEnergy[i]/eV),5)   
        - 1.86882e-10 * pow(1240./(fPhotonEnergy[i]/eV),6)
        + 1.89628e-13 * pow(1240./(fPhotonEnergy[i]/eV),7)    
        - 1.09704e-16 * pow(1240./(fPhotonEnergy[i]/eV),8)  
        + 2.76026e-20 * pow(1240./(fPhotonEnergy[i]/eV),9);
    PMQuantumResponseU03[i] /= (1 - 2*NormalReflection*(1-NormalReflection)); 
    Zero[i] = 2*NormalReflection*(1-NormalReflection);
    PMQuantumResponseU03[i] = (PMQuantumResponseU03[i] > 0 ? PMQuantumResponseU03[i] : 0.);
    PMQuantumResponseU03[i] = (1240./(fPhotonEnergy[i]/eV) > 182. && 1240./(fPhotonEnergy[i]/eV) < 650. ? PMQuantumResponseU03[i] : 0.);
    QuartzTransmission[i] += 2*NormalReflection*(1-NormalReflection);
#endif
    fQuartzAbsorptionLength[i]=(QuartzTransmission[i] > 1 ? 1e6*mm : (QuartzTransmission[i] > 0 ? 1.*mm/log(1./QuartzTransmission[i]) : 0.));
    /*
    G4cout.precision(8);
    G4cout << fPhotonEnergy[i]/eV << " nNe= " << fNeonRefIndex[i] << " nSiO2= " << fQuartzRefIndex[i] 
        << " Refl=" << 2.*NormalReflection*(1.-NormalReflection)
        << " Transm=" << QuartzTransmission[i] << " AbsL=" << fQuartzAbsorptionLength[i]/mm
        << " MRefl=" << AlMgF2GlassReflectivity[i] << " MyRefl=" << AlMgF2MylarReflectivity[i] << " QE=" << PMQuantumResponseU03[i] << G4endl;
    */
    fVacuumRefIndex[i]=1.;

    fMirrorReflectivity[i] = AlMgF2GlassReflectivity[i];
    fMirrorEfficiency[i] = 0.;
    MirrorSpecularLobe[i] = RICHML;
    MirrorSpecularSpike[i] = 1.0 - MirrorSpecularLobe[i];

    fConeReflectivity[i] = AlMgF2MylarReflectivity[i];
    fConeEfficiency[i] = 0.;

    fPMTsWindowReflectivity[i] = RICHFR; //FlaRefl
    fPMTsWindowEfficiency[i] = 0.;
    fPMTsWindowSpecularSpike[i] = RICHFSp; //FlaSpike
    fPMTsWindowSpecularLobe[i] = RICHFL; //FlaLobe
    fPMTsWindowBackscatter[i] = 0.;

    fVesselReflectivity[i] = RICHTR; //TubeRefl
    fVesselEfficiency[i] = 0.;
    fVesselSpecularSpike[i] = RICHTSp; //TubeSpike
    fVesselSpecularLobe[i] = RICHTL; //TubeLobe
    fVesselBackscatter[i] = 0.; //TubeBS

    if( i > 0 ){
      G4double Weight=370*(1-1/(fNeonRefIndex[i]*fNeonRefIndex[i]))*(fPhotonEnergy[i]-fPhotonEnergy[i-1])/eV
	*PMQuantumResponseU03[i]*perCent
	*QuartzTransmission[i]
	*fMirrorReflectivity[i]
	*fConeReflectivity[i]
	*TrackPathLengthInNeon/cm;
      MeanNeonRefIndex+=fNeonRefIndex[i]*Weight;
      NeonPhotonNumber+=Weight;
    }
  }

  if(NeonPhotonNumber) fMeanNeonRefIndex = MeanNeonRefIndex/NeonPhotonNumber; // needed by fast sim

  /*
  G4cout.precision(14);
  G4cout << "Expected Value of Neon Refractive Index: " << MeanNeonRefIndex/NeonPhotonNumber 
	 << " over " << NeonPhotonNumber << " Mean Detectable Photons" <<G4endl;
  */

  fNeonMPT = new G4MaterialPropertiesTable();
  fNeonMPT->AddProperty("RINDEX",fPhotonEnergy, fNeonRefIndex, fMaterialPropertiesNEntries);
  fNeonMPT->AddProperty("ABSLENGTH",    fPhotonEnergy, fNeonAbsorptionLength,     fMaterialPropertiesNEntries);
  fNeonMPT->AddProperty("FASTCOMPONENT",fPhotonEnergy, fNeonScintilFast,     fMaterialPropertiesNEntries);

  fNeonMPT->AddConstProperty("SCINTILLATIONYIELD",0.*2.*100./MeV);
  fNeonMPT->AddConstProperty("RESOLUTIONSCALE",1.0);
  fNeonMPT->AddConstProperty("FASTTIMECONSTANT", 3*ns);
//  fNeonMPT->AddConstProperty("SLOWTIMECONSTANT",10.*ns);
  fNeonMPT->AddConstProperty("YIELDRATIO",1.);

  fQuartzMPT = new G4MaterialPropertiesTable();
  fQuartzMPT->AddProperty("RINDEX",fPhotonEnergy, fQuartzRefIndex, fMaterialPropertiesNEntries);
  fQuartzMPT->AddProperty("ABSLENGTH",fPhotonEnergy, fQuartzAbsorptionLength, fMaterialPropertiesNEntries);

  fVacuumMPT = new G4MaterialPropertiesTable();
  fVacuumMPT->AddProperty("RINDEX",fPhotonEnergy, fVacuumRefIndex, fMaterialPropertiesNEntries);

  SetMirrorWindowInnerFlangeMaterial(G4Material::GetMaterial("G4_Al"));
  SetMirrorWindowOuterFlangeMaterial(G4Material::GetMaterial("G4_Al"));
  SetInterfaceRingMaterial(G4Material::GetMaterial("G4_Al"));


  // Define reflectivity properties of surface
  fMirrorOpticalSurfaceSigmaAlpha = RICHMS*rad; //MirSigma
  fMirrorOpticalSurfacePT = new G4MaterialPropertiesTable();
  fMirrorOpticalSurfacePT -> AddProperty("REFLECTIVITY",fPhotonEnergy,fMirrorReflectivity,
		                       fMaterialPropertiesNEntries);
  fMirrorOpticalSurfacePT -> AddProperty("EFFICIENCY",fPhotonEnergy,fMirrorEfficiency,
					       fMaterialPropertiesNEntries);
  fMirrorOpticalSurfacePT->AddProperty("SPECULARSPIKECONSTANT",fPhotonEnergy, 
					  MirrorSpecularSpike, fMaterialPropertiesNEntries);
  fMirrorOpticalSurfacePT->AddProperty("SPECULARLOBECONSTANT",fPhotonEnergy, 
					  MirrorSpecularLobe, fMaterialPropertiesNEntries);

  fConeOpticalSurfacePT = new G4MaterialPropertiesTable();
  fConeOpticalSurfacePT -> AddProperty("REFLECTIVITY",fPhotonEnergy,fConeReflectivity,
		                       fMaterialPropertiesNEntries);
  fConeOpticalSurfacePT -> AddProperty("EFFICIENCY",fPhotonEnergy,fConeEfficiency,
					       fMaterialPropertiesNEntries);

  fPMTsWindowOpticalSurfaceSigmaAlpha = RICHFS*rad; //FlaSigma
  fPMTsWindowOpticalSurfacePT = new G4MaterialPropertiesTable();
  fPMTsWindowOpticalSurfacePT->AddProperty("REFLECTIVITY",fPhotonEnergy,
					  fPMTsWindowReflectivity,fMaterialPropertiesNEntries);
  fPMTsWindowOpticalSurfacePT->AddProperty("EFFICIENCY",fPhotonEnergy,
					  fPMTsWindowEfficiency,fMaterialPropertiesNEntries);
  fPMTsWindowOpticalSurfacePT->AddProperty("SPECULARSPIKECONSTANT",fPhotonEnergy, 
					  fPMTsWindowSpecularSpike, fMaterialPropertiesNEntries);
  fPMTsWindowOpticalSurfacePT->AddProperty("SPECULARLOBECONSTANT", fPhotonEnergy, 
					  fPMTsWindowSpecularLobe,  fMaterialPropertiesNEntries);
  fPMTsWindowOpticalSurfacePT->AddProperty("BACKSCATTERCONSTANT",  fPhotonEnergy, 
					  fPMTsWindowBackscatter,   fMaterialPropertiesNEntries);

  fVesselOpticalSurfaceSigmaAlpha = RICHTS*rad; //TubeSigma
  fVesselOpticalSurfacePT = new G4MaterialPropertiesTable();
  fVesselOpticalSurfacePT->AddProperty("REFLECTIVITY",fPhotonEnergy,
				     fVesselReflectivity,fMaterialPropertiesNEntries);
  fVesselOpticalSurfacePT->AddProperty("EFFICIENCY",fPhotonEnergy,
				     fVesselEfficiency,fMaterialPropertiesNEntries);
  fVesselOpticalSurfacePT->AddProperty("SPECULARSPIKECONSTANT",fPhotonEnergy, 
					  fVesselSpecularSpike, fMaterialPropertiesNEntries);
  fVesselOpticalSurfacePT->AddProperty("SPECULARLOBECONSTANT", fPhotonEnergy, 
					  fVesselSpecularLobe,  fMaterialPropertiesNEntries);
  fVesselOpticalSurfacePT->AddProperty("BACKSCATTERCONSTANT",  fPhotonEnergy, 
					  fVesselBackscatter,   fMaterialPropertiesNEntries);

  G4int NPMs = RICHGeometryParameters::GetInstance()->GetNPMs();
  fPhotocatodePT = new G4MaterialPropertiesTable*[NPMs];
  fPMType = new G4String[NPMs];
  fPMPeakQE = new G4double[NPMs];
  fPMQuantumEfficiency = new G4double*[NPMs];

  ReadPMsData(NPMs);

  for(G4int iPM = 0; iPM < NPMs; iPM++){
    fPMQuantumEfficiency[iPM] = new G4double[fMaterialPropertiesNEntries];
    for(G4int i = 0; i < fMaterialPropertiesNEntries; i++)    
      //fPMQuantumEfficiency[iPM][i]=perCent*(fPMType[iPM] == "U06" ? PMQuantumResponseU06[i] : PMQuantumResponseU03[i])
      fPMQuantumEfficiency[iPM][i] = 1
#ifdef RICHTestOPT
	*1.;
#else
        *1.;
	//*(fPMPeakQE[iPM] >= 0 ? fPMPeakQE[iPM]/20.1 : 1.);
#endif
    fPhotocatodePT[iPM] = new G4MaterialPropertiesTable();
    fPhotocatodePT[iPM] -> AddProperty("EFFICIENCY",fPhotonEnergy,
				       fPMQuantumEfficiency[iPM],fMaterialPropertiesNEntries);
    fPhotocatodePT[iPM] -> AddProperty("REFLECTIVITY",fPhotonEnergy,
				       Zero,fMaterialPropertiesNEntries);
  }
  DefineMaterials();
  SetMaterialProperties();
}

RICHMaterialParameters::~RICHMaterialParameters(){}

RICHMaterialParameters* RICHMaterialParameters::GetInstance()
{
  if ( fInstance == 0 ) { fInstance = new RICHMaterialParameters(); }
//   fInstance->Print();
  return fInstance;
}

void RICHMaterialParameters::ReadPMsData(G4int /*NPMs*/)
{
}

void RICHMaterialParameters::DefineMaterials()
{

  // This method creates a set of materials which will be used to build
  // the detector. It uses the predefined NIST-derived materials list
  // andd adds to it any NA48-specific material.

  G4NistManager* nistMgr = G4NistManager::Instance();

  // Use these if you want extensive output about materials definition
  //nistMgr->SetVerbose(1);
  //nistMgr->PrintElement("all");
  //nistMgr->ListMaterials("all");


  // Standard materials
  G4cout << nistMgr->FindOrBuildMaterial("G4_Ne") << G4endl;
  nistMgr->FindOrBuildMaterial("G4_AIR");
  nistMgr->FindOrBuildMaterial("G4_Galactic");


  nistMgr->FindOrBuildMaterial("G4_Fe");
  nistMgr->FindOrBuildMaterial("G4_Pb");
  nistMgr->FindOrBuildMaterial("G4_Cr");
  nistMgr->FindOrBuildMaterial("G4_Ni");
  nistMgr->FindOrBuildMaterial("G4_Al");
  nistMgr->FindOrBuildMaterial("G4_Kr");
  nistMgr->FindOrBuildMaterial("G4_GLASS_PLATE");
  nistMgr->FindOrBuildMaterial("G4_MYLAR");
  nistMgr->FindOrBuildMaterial("G4_SILICON_DIOXIDE");
  nistMgr->FindOrBuildMaterial("G4_PLASTIC_SC_VINYLTOLUENE");
  nistMgr->FindOrBuildMaterial("G4_WATER");
  nistMgr->FindOrBuildMaterial("G4_POLYVINYL_CHLORIDE");

  G4Element* O  = new G4Element("Oxygen"  ,"O" , 8., 16.00*g/mole);
  G4Element* Si = new G4Element("Silicon","Si" , 14., 28.09*g/mole);
  G4Element* H  = new G4Element("Hydrogen","H" , 1., 1.01*g/mole);
  G4Element* C  = new G4Element("Carbon"  ,"C" , 6., 12.01*g/mole);

  // Neon
  G4Material* RICHNeon = new G4Material("RICH_Ne",G4Material::GetMaterial("G4_Ne")->GetDensity(),1);
  RICHNeon->AddMaterial(G4Material::GetMaterial("G4_Ne"), 100.0*perCent); 

  // Quartz (il Silicon_dioxide di G4 ha densità = 2.32 g/cm3)
  G4Material* RICHQuartz = new G4Material("RICH_SILICON_DIOXIDE",G4Material::GetMaterial("G4_SILICON_DIOXIDE")->GetDensity(),1);
  RICHQuartz->AddMaterial(G4Material::GetMaterial("G4_SILICON_DIOXIDE"), 100.0*perCent); 

  // Glass (per la fibra di vetro)
  G4Material* SiO2 = new G4Material("RICH_Glass",2.200*g/cm3,2);
  SiO2->AddElement(Si,1);
  SiO2->AddElement(O,2);


  // Vacuum
  G4Material* RICHVacuum = new G4Material("RICH_Galactic",G4Material::GetMaterial("G4_Galactic")->GetDensity(),1);
  RICHVacuum->AddMaterial(G4Material::GetMaterial("G4_Galactic"), 100.0*perCent); 

  // Stainless Steel (StainlessSteel)
  G4Material* StainlessSteel = new G4Material("RICH_StainlessSteel",7.88*g/cm3,4);
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Fe"), 71.5*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Cr"), 18.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Ni"), 10.0*perCent); 
  StainlessSteel->AddMaterial(G4Material::GetMaterial("G4_Si"),  0.5*perCent); 

  //Epoxy (dipende dalla formulazione)

  G4Material* Epoxy = new G4Material("RICH_Epoxy", 1.15*g/cm3, 3);
  Epoxy->AddElement(C, 18);
  Epoxy->AddElement(H, 20);
  Epoxy->AddElement(O, 3);

  //Stesalite (Glass(70%) + Epoxy(30%))

  G4Material* Stesalite= new G4Material("RICH_Stesalite", 1.89*g/cm3, 2);
  Stesalite->AddMaterial(SiO2, 0.82);
  Stesalite->AddMaterial(Epoxy, 0.18);

}

void RICHMaterialParameters::SetMaterialProperties()
{
  G4Material* Neon= G4Material::GetMaterial("RICH_Ne");

  Neon->SetMaterialPropertiesTable(fNeonMPT);

  G4Material* Quartz= G4Material::GetMaterial("RICH_SILICON_DIOXIDE");

  Quartz->SetMaterialPropertiesTable(fQuartzMPT);

  G4Material* Vacuum= G4Material::GetMaterial("RICH_Galactic");

  Vacuum->SetMaterialPropertiesTable(fVacuumMPT);

}

TObjArray RICHMaterialParameters::GetHashTable()
{
  TObjArray RICHMaterialParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fMaterialPropertiesNEntries;
  Value = Buffer.str();
  Buffer.str("");
  Float_t MaterialPropertiesNEntries = fMaterialPropertiesNEntries;
  ParameterData.Add(new TVector(1, &MaterialPropertiesNEntries));
  RICHMaterialParameters.Add(new DetectorParameter("fMaterialPropertiesNEntries",Value.Data(),
					       "Material Properties N Entries", ParameterData));
  ParameterData.Clear();

  Buffer << fMinWavelength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fMinWavelength));
  RICHMaterialParameters.Add(new DetectorParameter("fMinWavelength",Value.Data(),
					       "Min Wavelength", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fPhotonEnergy[i]/eV;
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fPhotonEnergy));
  RICHMaterialParameters.Add(new DetectorParameter("fPhotonEnergy",Value.Data(),
					       "Photon Energy", ParameterData));
  ParameterData.Clear();

  Buffer.precision(10);
  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fNeonRefIndex[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  Buffer.precision(-1);
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fNeonRefIndex));
  RICHMaterialParameters.Add(new DetectorParameter("fNeonRefIndex",Value.Data(),
					       "Neon Ref Index", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fNeonScintilFast[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fNeonScintilFast));
  RICHMaterialParameters.Add(new DetectorParameter("fNeonScintilFast",Value.Data(),
					       "Neon Scintil Fast", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fNeonAbsorptionLength[i]/m;
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fNeonAbsorptionLength));
  RICHMaterialParameters.Add(new DetectorParameter("fNeonAbsorptionLength",Value.Data(),
					       "Neon Absorption Length", ParameterData));
  ParameterData.Clear();

  Buffer << fNeonPressure/bar*1000;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fNeonPressure));
  RICHMaterialParameters.Add(new DetectorParameter("fNeonPressure",Value.Data(),
					       "Neon Pressure", ParameterData));
  ParameterData.Clear();

  Buffer << fNeonTemperature;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fNeonTemperature));
  RICHMaterialParameters.Add(new DetectorParameter("fNeonTemperature",Value.Data(),
					       "Neon Temperature", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fQuartzRefIndex[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fQuartzRefIndex));
  RICHMaterialParameters.Add(new DetectorParameter("fQuartzRefIndex",Value.Data(),
					       "Quartz Ref Index", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fQuartzAbsorptionLength[i]/m;
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fQuartzAbsorptionLength));
  RICHMaterialParameters.Add(new DetectorParameter("fQuartzAbsorptionLength",Value.Data(),
					       "Quartz Absorption Length", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fVacuumRefIndex[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fVacuumRefIndex));
  RICHMaterialParameters.Add(new DetectorParameter("fVacuumRefIndex",Value.Data(),
					       "Vacuum Ref Index", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fMirrorReflectivity[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fMirrorReflectivity));
  RICHMaterialParameters.Add(new DetectorParameter("fMirrorReflectivity",Value.Data(),
					       "Mirror Reflectivity", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fMirrorEfficiency[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fMirrorEfficiency));
  RICHMaterialParameters.Add(new DetectorParameter("fMirrorEfficiency",Value.Data(),
					       "Mirror Efficiency", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fConeReflectivity[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fConeReflectivity));
  RICHMaterialParameters.Add(new DetectorParameter("fConeReflectivity",Value.Data(),
					       "Cone Reflectivity", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fConeEfficiency[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fConeEfficiency));
  RICHMaterialParameters.Add(new DetectorParameter("fConeEfficiency",Value.Data(),
					       "Cone Efficiency", ParameterData));
  ParameterData.Clear();

  Buffer << fPMTsWindowOpticalSurfaceSigmaAlpha;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fPMTsWindowOpticalSurfaceSigmaAlpha));
  RICHMaterialParameters.Add(new DetectorParameter("fPMTsWindowOpticalSurfaceSigmaAlpha",Value.Data(),
					       "PMTs Window Optical Surface Sigma Alpha", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fPMTsWindowReflectivity[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fPMTsWindowReflectivity));
  RICHMaterialParameters.Add(new DetectorParameter("fPMTsWindowReflectivity",Value.Data(),
					       "PMTs Window Reflectivity", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fPMTsWindowEfficiency[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fPMTsWindowEfficiency));
  RICHMaterialParameters.Add(new DetectorParameter("fPMTsWindowEfficiency",Value.Data(),
					       "PMTs Window Efficiency", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fPMTsWindowSpecularSpike[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fPMTsWindowSpecularSpike));
  RICHMaterialParameters.Add(new DetectorParameter("fPMTsWindowSpecularSpike",Value.Data(),
					       "PMTs Window Specular Spike", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fPMTsWindowSpecularLobe[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fPMTsWindowSpecularLobe));
  RICHMaterialParameters.Add(new DetectorParameter("fPMTsWindowSpecularLobe",Value.Data(),
					       "PMTs Window Specular Lobe", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fPMTsWindowBackscatter[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fPMTsWindowBackscatter));
  RICHMaterialParameters.Add(new DetectorParameter("fPMTsWindowBackscatter",Value.Data(),
					       "PMTs Window Backscatter", ParameterData));
  ParameterData.Clear();

  Buffer << fVesselOpticalSurfaceSigmaAlpha;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fVesselOpticalSurfaceSigmaAlpha));
  RICHMaterialParameters.Add(new DetectorParameter("fVesselOpticalSurfaceSigmaAlpha",Value.Data(),
					       "Vessel Optical Surface Sigma Alpha", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fVesselReflectivity[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fVesselReflectivity));
  RICHMaterialParameters.Add(new DetectorParameter("fVesselReflectivity",Value.Data(),
					       "Vessel Reflectivity", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fVesselEfficiency[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fVesselEfficiency));
  RICHMaterialParameters.Add(new DetectorParameter("fVesselEfficiency",Value.Data(),
					       "Vessel Efficiency", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fVesselSpecularSpike[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fVesselSpecularSpike));
  RICHMaterialParameters.Add(new DetectorParameter("fVesselSpecularSpike",Value.Data(),
					       "Vessel Specular Spike", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fVesselSpecularLobe[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fVesselSpecularLobe));
  RICHMaterialParameters.Add(new DetectorParameter("fVesselSpecularLobe",Value.Data(),
					       "Vessel Specular Lobe", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < fMaterialPropertiesNEntries; i++){
    Buffer << fVesselBackscatter[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(fMaterialPropertiesNEntries, fVesselBackscatter));
  RICHMaterialParameters.Add(new DetectorParameter("fVesselBackscatter",Value.Data(),
					       "Vessel Backscatter", ParameterData));
  ParameterData.Clear();


  G4int NPMs = RICHGeometryParameters::GetInstance()->GetNPMs();
  for(G4int i = 0; i < NPMs; i++){
    ParameterData.Add(new TObjString(fPMType[i].c_str()));
    Buffer << fPMType[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  RICHMaterialParameters.Add(new DetectorParameter("fPMType",Value.Data(),
					       "P M Type", ParameterData));
  ParameterData.Clear();


  for(G4int i = 0; i < NPMs; i++){
    Buffer << fPMPeakQE[i];
    Buffer << " ";
    if((i+1)%8==0)
      Buffer << G4endl;
  }
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(NPMs, fPMPeakQE));
  RICHMaterialParameters.Add(new DetectorParameter("fPMPeakQE",Value.Data(),
					       "P M Peak Q E", ParameterData));
  ParameterData.Clear();


//   for(G4int i = 0; i < NPMs; i++){
//     Buffer << fPMQuantumEfficiency[i];
//     if((i+1)%8==0)
//       Buffer << G4endl;
//     else
//       Buffer << " ";
//   }
//   Value = Buffer.str();
//   Buffer.str("");
//   ParameterData.Add(new TVectorT<G4double>(NPMs, fPMQuantumEfficiency));
//   RICHMaterialParameters.Add(new DetectorParameter("fPMQuantumEfficiency",Value.Data(),
// 					       "P M Quantum Efficiency", ParameterData));
//   ParameterData.Clear();

  return RICHMaterialParameters;
}

void RICHMaterialParameters::Print(){
  G4cout << "fMaterialPropertiesNEntries= "<< fMaterialPropertiesNEntries << G4endl
	 << "fMinWavelength= "<< fMinWavelength << G4endl
	 << "fPhotonEnergy= "<< fPhotonEnergy << G4endl
	 << "fNeonRefIndex= "<< fNeonRefIndex << G4endl
	 << "fNeonScintilFast= "<< fNeonScintilFast << G4endl
	 << "fNeonAbsorptionLength= "<< fNeonAbsorptionLength << G4endl
	 << "fNeonPressure= "<< fNeonPressure << G4endl
	 << "fNeonTemperature= "<< fNeonTemperature << G4endl
	 << "fQuartzRefIndex= "<< fQuartzRefIndex << G4endl
	 << "fQuartzAbsorptionLength= "<< fQuartzAbsorptionLength << G4endl
	 << "fVacuumRefIndex= "<< fVacuumRefIndex << G4endl
	 << "fMirrorReflectivity= "<< fMirrorReflectivity << G4endl
	 << "fMirrorEfficiency= "<< fMirrorEfficiency << G4endl
	 << "fConeReflectivity= "<< fConeReflectivity << G4endl
	 << "fConeEfficiency= "<< fConeEfficiency << G4endl
	 << "fPMTsWindowOpticalSurfaceSigmaAlpha= "<< fPMTsWindowOpticalSurfaceSigmaAlpha << G4endl
	 << "fPMTsWindowReflectivity= "<< fPMTsWindowReflectivity << G4endl
	 << "fPMTsWindowEfficiency= "<< fPMTsWindowEfficiency << G4endl
	 << "fPMTsWindowSpecularSpike= "<< fPMTsWindowSpecularSpike << G4endl
	 << "fPMTsWindowSpecularLobe= "<< fPMTsWindowSpecularLobe << G4endl
	 << "fPMTsWindowBackscatter= "<< fPMTsWindowBackscatter << G4endl
	 << "fVesselOpticalSurfaceSigmaAlpha= "<< fVesselOpticalSurfaceSigmaAlpha << G4endl
	 << "fVesselReflectivity= "<< fVesselReflectivity << G4endl
	 << "fVesselEfficiency= "<< fVesselEfficiency << G4endl
	 << "fVesselSpecularSpike= "<< fVesselSpecularSpike << G4endl
	 << "fVesselSpecularLobe= "<< fVesselSpecularLobe << G4endl
	 << "fVesselBackscatter= "<< fVesselBackscatter << G4endl
	 << "fPMType= "<< fPMType << G4endl
	 << "fPMPeakQE= "<< fPMPeakQE << G4endl
	 << "fPMQuantumEfficiency= "<< fPMQuantumEfficiency << G4endl
	 << "fNeonMPT= "<< fNeonMPT << G4endl
	 << "fQuartzMPT= "<< fQuartzMPT << G4endl
	 << "fVacuumMPT= "<< fVacuumMPT << G4endl
	 << "fMirrorOpticalSurfacePT= "<< fMirrorOpticalSurfacePT << G4endl
	 << "fConeOpticalSurfacePT= "<< fConeOpticalSurfacePT << G4endl
	 << "fPMTsWindowOpticalSurfacePT= "<< fPMTsWindowOpticalSurfacePT << G4endl
	 << "fVesselOpticalSurfacePT= "<< fVesselOpticalSurfacePT << G4endl
	 << "fPhotocatodePT= "<< fPhotocatodePT << G4endl;
}
