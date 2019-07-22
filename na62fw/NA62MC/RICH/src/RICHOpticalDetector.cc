#include "G4Tubs.hh"
#include "G4Cons.hh"
#include "G4Polycone.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "RICHGeometryParameters.hh"

#include "RICHOpticalDetector.hh"

#include "G4LogicalBorderSurface.hh"
#include "G4OpticalSurface.hh"
#include "RICHMaterialParameters.hh"

#include "RICHPMTSD.hh"
#include "G4SDManager.hh"

RICHOpticalDetector::RICHOpticalDetector(G4LogicalVolume * MotherVolume, G4Material* Material) : NA62VComponent(Material, MotherVolume)
{
  ReadGeometryParameters();
  CreateGeometry();
  DefineOpticalSurfaces();
  SetProperties();
}

RICHOpticalDetector::~RICHOpticalDetector(){}

void RICHOpticalDetector::ReadGeometryParameters()
{
  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();
  fConeInputRadius = GeoPars->GetConeInputRadius();
  fConeOutputRadius = GeoPars->GetConeOutputRadius();
  fConeLongitudinalLength = GeoPars->GetConeLongitudinalLength();
  
  fMylarConeThickness = GeoPars->GetMylarConeThickness();
  fMylarConeInputRadius = GeoPars->GetMylarConeInputRadius();
  fMylarConeOutputRadius = GeoPars->GetMylarConeOutputRadius();
  fMylarConeLongitudinalLength = GeoPars->GetMylarConeLongitudinalLength();

  fQuartzWindowInnerRadius = GeoPars->GetQuartzWindowInnerRadius();
  fQuartzWindowOuterRadius = GeoPars->GetQuartzWindowOuterRadius();
  fQuartzWindowThickness = GeoPars->GetQuartzWindowThickness();
  
  fPMWindowInnerRadius = GeoPars->GetPMWindowInnerRadius();
  fPMWindowOuterRadius = GeoPars->GetPMWindowOuterRadius();
  fPMWindowThickness = GeoPars->GetPMWindowThickness();

  fPMOuterRadius = GeoPars->GetPMOuterRadius();
  fPMLength = GeoPars->GetPMLength();

  fPMsPositions = GeoPars->GetPMsPositions();
}

void RICHOpticalDetector::CreateGeometry()
{
  G4double HalfTotalZLength = 0.5*(RICHGeometryParameters::GetInstance()->GetPMTsDiskZLength()+fPMWindowThickness+fPMLength);
  G4double startPhiAngle=0;
  G4double deltaPhiAngle=360*deg;

    G4Cons * solidMylarCone = new G4Cons("MylarCone",fMylarConeOutputRadius,fConeOutputRadius,
					 fMylarConeInputRadius,fConeInputRadius,
					 0.5*fConeLongitudinalLength,startPhiAngle,deltaPhiAngle);
    
    G4LogicalVolume * logicMylarCone = new G4LogicalVolume(solidMylarCone,
					  G4Material::GetMaterial("G4_MYLAR"),
					  "logicMylarCone",
					  0,0,0);

    G4Tubs * solidQuartzWindow = new G4Tubs("solidQuartzWindow",fQuartzWindowInnerRadius,fQuartzWindowOuterRadius,
					    0.5*fQuartzWindowThickness,startPhiAngle,deltaPhiAngle);
    
    G4LogicalVolume * logicQuartzWindow = new G4LogicalVolume(solidQuartzWindow,
					                     G4Material::GetMaterial("RICH_SILICON_DIOXIDE"),
					                     "logicQuartzWindow",
					                     0,0,0);

    G4Tubs * solidPMWindow = new G4Tubs("solidPMWindow",fPMWindowInnerRadius,fPMWindowOuterRadius,
					0.5*fPMWindowThickness,startPhiAngle,deltaPhiAngle);
    
    G4LogicalVolume * logicPMWindow = new G4LogicalVolume(solidPMWindow,
					 G4Material::GetMaterial("RICH_SILICON_DIOXIDE"),
					 "logicPMWindow",
					 0,0,0);

    G4Tubs * solidPhotocatode = new G4Tubs("solidPhotocatode",fPMWindowInnerRadius,fPMWindowOuterRadius,
					 0.5*50*um,startPhiAngle,deltaPhiAngle);
  
    G4LogicalVolume * logicPhotocatode = new G4LogicalVolume(solidPhotocatode,
					    G4Material::GetMaterial("RICH_Galactic"),
					    "logicPhotocatode",
					    0,0,0);

    G4double zPM[4]={
    HalfTotalZLength-fConeLongitudinalLength-fQuartzWindowThickness,
    HalfTotalZLength-fConeLongitudinalLength-fQuartzWindowThickness-2*mm-50*um,
    HalfTotalZLength-fConeLongitudinalLength-fQuartzWindowThickness-2*mm-50*um,
    -HalfTotalZLength}; 
  G4double rMinPM[4]={fPMWindowOuterRadius,fPMWindowOuterRadius,0,0};
  G4double rMaxPM[4]={
    fPMOuterRadius,
    fPMOuterRadius,
    fPMOuterRadius,
    fPMOuterRadius};

  G4Polycone * solidPMCase = new G4Polycone("solidPMCase",0,360*deg,4,zPM,rMinPM,rMaxPM);

  G4LogicalVolume* LogicPMCase= new G4LogicalVolume(solidPMCase,
						    G4Material::GetMaterial("G4_POLYVINYL_CHLORIDE"),   // material
						    "PMCase",          // name
						    0,                    // field manager
						    0,                    // sensitive detector
						    0);                   // user limits

  G4double zCone[6]={
    HalfTotalZLength,
    HalfTotalZLength-fConeLongitudinalLength,
    HalfTotalZLength-fConeLongitudinalLength,
    HalfTotalZLength-fConeLongitudinalLength-0.5*fQuartzWindowThickness,
    HalfTotalZLength-fConeLongitudinalLength-0.5*fQuartzWindowThickness,
    -HalfTotalZLength};
    G4double rMinCone[6]={0,0,0,0,0,0};
    G4double rMaxCone[6]={
      fConeInputRadius,
      fConeOutputRadius,
      fQuartzWindowOuterRadius,
      fQuartzWindowOuterRadius,
      fPMOuterRadius,
      fPMOuterRadius};

   G4Polycone * Hole= new G4Polycone("Hole",0,360*deg,6,zCone,rMinCone,rMaxCone);
 
    G4double zNe[2]={-fConeLongitudinalLength,
		     0};
     G4double rMinNe[2]={0,0};
     G4double rMaxNe[2]={fMylarConeOutputRadius,
 			fMylarConeInputRadius};
     
     G4Polycone * solidInnerCone= new G4Polycone("InnerCone",0,360*deg,2,zNe,rMinNe,rMaxNe);
    
    G4LogicalVolume * logicConeRadiator = new G4LogicalVolume(solidInnerCone,      // solid
					     G4Material::GetMaterial("RICH_Ne"),                   // material
					     "logicConeRadiator",          // name
					     0,                    // field manager
					     0,                    // sensitive detector
					     0);                   // user limits

  fLogicHole= new G4LogicalVolume(Hole,
				  G4Material::GetMaterial("RICH_Galactic"),   // material
				  "Hole",          // name
				  0,                    // field manager
				  0,                    // sensitive detector
				  0);                   // user limits

  fPhysicalQuartzWindow = new G4PVPlacement(0,                 //Spots
					    G4ThreeVector(0,0,
							  HalfTotalZLength
							  -fConeLongitudinalLength
							  -0.5*fQuartzWindowThickness),
					    logicQuartzWindow,
					    "physiQuartzWindow",         // its name
					    fLogicHole,               // its mother  volume
					    true,           // no boolean operations
					    0);              // copy number

  fPhysicalPMWindow = new G4PVPlacement(0,                     //Spots
					G4ThreeVector(0,0,
						      HalfTotalZLength
						      -fConeLongitudinalLength            
						      -fQuartzWindowThickness	              
						      -1*mm			              
						      -0.5*fPMWindowThickness),	              
					logicPMWindow,				   	      
					"physiPMWindow",         // its name	              
					fLogicHole,               // its mother  volume           
					false,           // no boolean operations	   
					0);              // copy number

  fPhysicalMylarCone = new G4PVPlacement(0,
					 G4ThreeVector(0,
						       0,
						       HalfTotalZLength
						       -0.5*fConeLongitudinalLength),
					 logicMylarCone,
					 "MylarCone",         // its name
					 fLogicHole,               // its mother  volume
					 false,           // no boolean operations
					 0);              // copy number
  
  fPhysicalConeRadiator = new G4PVPlacement(0,               // no rotation
					    G4ThreeVector(0,
							  0,
							  HalfTotalZLength),
					    logicConeRadiator,      // its logical volume
					    "ConeRadiator",         // its name
					    fLogicHole,               // its mother  volume
					    false,           // no boolean operations
					    0);              // copy number


  fPhysicalPhotocatode = new G4PVPlacement(0,                     //Spots
					   G4ThreeVector(0,0,
							 HalfTotalZLength
							 -fConeLongitudinalLength	              
							 -fQuartzWindowThickness	             
							 -1*mm
							 -fPMWindowThickness
							 -50.*um              
							 -0.5*50*um),
					   logicPhotocatode,			   	      
					   "physiPhotocatode",         // its name	              
					   fLogicHole,              // its mother  volume           
					   false,           // no boolean operations	   
					   0);              // copy number

  new G4PVPlacement(0,
		    G4ThreeVector(),
		    LogicPMCase,
		    "PMCase",
		    fLogicHole,
		    false,
		    0);

  // Full G4 simulation and for any charged particle reaching the RICH Pms: Sensitive detector at PM Flange

  G4SDManager* SDman = G4SDManager::GetSDMpointer();

  G4String pmtSDname = "/PMTs";
  G4String PMTCollectionName= "PMTCollection";
  RICHPMTSD * pmtSD = static_cast<RICHPMTSD*>(SDman->FindSensitiveDetector(pmtSDname));
  if(!pmtSD){  
    pmtSD = new RICHPMTSD(pmtSDname,PMTCollectionName);
    SDman->AddNewDetector(pmtSD);
  }
  logicPMWindow->SetSensitiveDetector(pmtSD);                                                                                        
}

void RICHOpticalDetector::DefineOpticalSurfaces()
{
  // Photocatode surface
  fPhotocatodeOpticalSurface = new G4OpticalSurface("Photocatode");
  fPhotocatodeOpticalSurface->SetType(dielectric_metal);
  fPhotocatodeOpticalSurface->SetFinish(polished);
  fPhotocatodeOpticalSurface->SetModel(glisur);
  fPhotocatodeOpticalSurface -> SetMaterialPropertiesTable(RICHMaterialParameters::GetInstance()
							   ->GetPhotocatodePT(0));
  new G4LogicalBorderSurface("Quartz/Photocatode Surface",
			     fPhysicalPMWindow,fPhysicalPhotocatode,fPhotocatodeOpticalSurface);

  // Cone surface
  fConeOpticalSurface = new G4OpticalSurface("Cone");
  fConeOpticalSurface->SetType(dielectric_metal);
  fConeOpticalSurface->SetFinish(polished);
  fConeOpticalSurface->SetModel(glisur);
  fConeOpticalSurface -> SetMaterialPropertiesTable(RICHMaterialParameters::GetInstance()
						    ->GetConeOpticalSurfacePT());
  new G4LogicalBorderSurface("Neon/Cone Surface",
			     fPhysicalConeRadiator,fPhysicalMylarCone,fConeOpticalSurface);                   
}

void RICHOpticalDetector::SetProperties()
{
  //fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  //fLogicalVolume ->SetVisAttributes(fVisAtt); 
}
