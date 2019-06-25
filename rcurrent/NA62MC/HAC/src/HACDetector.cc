#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"
#include "G4Transform3D.hh"

#include "BeamPipe.hh"

#include "HACGeometryParameters.hh"
#include "HACMaterialParameters.hh"

#include "HACDetector.hh"
#include "HACMagnet.hh"
#include "HACModule.hh"
#include "HACAbsorberLayer.hh"
#include "HACScintillatorLayer.hh"

#include "HACSD.hh"
#include "G4SDManager.hh"
//
// --------------------------------------------------------------------
// History:
//
// Created by Giuseppe Ruggiero 04-09-2012 
// 
// 11-01-2013 Spasimir Balev - added basic description of HAC detector
//
// --------------------------------------------------------------------

HACDetector::HACDetector(G4Material * Material, G4LogicalVolume * MotherVolume) : 
  NA62VComponent(Material,MotherVolume), NA62VNamed("HAC"),
  fHACRespRegionZCenter(.0),
  fHACRespRegionXLength(.0),
  fHACRespRegionYLength(.0),
  fHACRespRegionZLength(.0),
  fMagnetPosZ(.0),
  fHACDetectorYRotation(.0),
  fHACModuleZPosition(.0)
{
  // Mandatory here to Find or Build the needed materials
  HACMaterialParameters::GetInstance();
}

HACDetector::~HACDetector(){}

void HACDetector::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  HACGeometryParameters* GeoPars = HACGeometryParameters::GetInstance();
  fHACRespRegionZCenter = GeoPars->GetHACRespRegionZCenter();
  fHACRespRegionXLength = GeoPars->GetHACRespRegionXLength();
  fHACRespRegionYLength = GeoPars->GetHACRespRegionYLength();
  fHACRespRegionZLength = GeoPars->GetHACRespRegionZLength();
  fMagnetPosZ = GeoPars->GetHACMagnetZPosition()-fHACRespRegionZCenter; 
  fHACDetectorYRotation = GeoPars->GetHACDetectorYRotation();
  for(G4int i=0;i<9;i++) {
    fHACModuleXPosition[i] = GeoPars->GetHACModuleXPosition(i);
    fHACModuleYPosition[i] = GeoPars->GetHACModuleYPosition(i);
  }
  fHACModuleZPosition = GeoPars->GetHACDetectorZPosition() - fHACRespRegionZCenter + GeoPars->GetHACModuleZLength()/2.;
}

void HACDetector::CreateGeometry()
{
  ReadGeometryParameters();
  HACGeometryParameters* GeoPars = HACGeometryParameters::GetInstance();

  // Define sensitive detector
  G4SDManager* SDman = G4SDManager::GetSDMpointer();
  G4String HACSensitiveDetectorName = "/HAC";
  G4String HACCollectionName= "HACCollection";
  HACSD * HacSD = static_cast<HACSD*>(SDman->FindSensitiveDetector(HACSensitiveDetectorName));
  if(!HacSD)
  {
    HacSD = new HACSD(HACSensitiveDetectorName,HACCollectionName);
    SDman->AddNewDetector(HacSD);
  }

  // Define HAC responsibility region
  fSolidVolume= new G4Box("HAC",fHACRespRegionXLength/2.,fHACRespRegionYLength/2.,fHACRespRegionZLength/2.);
  fLogicalVolume= new G4LogicalVolume(fSolidVolume,fMaterial,"HAC",0,0,0);
  fPhysicalVolume = new G4PVPlacement(0,G4ThreeVector(0.,0.,fHACRespRegionZCenter),fLogicalVolume,"HAC",fMotherVolume,false,0);
  SetProperties();

  // Magnet
  G4ThreeVector MagnetPosition = G4ThreeVector(0.,0.,fMagnetPosZ); 
  new HACMagnet(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume,MagnetPosition);

  // Beam pipe
  new BeamPipe(0,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume);
  new BeamPipe(1,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume);
  new BeamPipe(2,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume);

  // Flange
  G4double flangeZ = 250.7365*m-fHACRespRegionZCenter;
  G4Tubs *HACFlange1 = new G4Tubs("HACFlange1",0,159*mm,2.5*mm,0,360*deg);
  G4LogicalVolume* HACFlange1Logical = new G4LogicalVolume(HACFlange1,G4Material::GetMaterial("G4_Fe"),"HACFlange1",0,0,0);
  new G4PVPlacement(0,G4ThreeVector(40*mm,0.,flangeZ),HACFlange1Logical,"HACFlange1",fLogicalVolume,false,0);
  G4Tubs *HACFlange = new G4Tubs("HACFlange",0,98.5*mm,2.5*mm,0,360*deg);
  G4LogicalVolume* HACFlangeLogical = new G4LogicalVolume(HACFlange,G4Material::GetMaterial("G4_Galactic"),"HACFlange",0,0,0);
  new G4PVPlacement(0,G4ThreeVector(-40.*mm,0.,0.),HACFlangeLogical,"HACFlange",HACFlange1Logical,false,0);

  // Place HAC modules
  G4RotationMatrix Rotation;
  Rotation.rotateX(0);
  Rotation.rotateY(fHACDetectorYRotation);
  Rotation.rotateZ(0);
  for(G4int iModule = 0;iModule<9;iModule++) {
    G4ThreeVector Position = G4ThreeVector(fHACModuleXPosition[iModule],
					   fHACModuleYPosition[iModule],
					   fHACModuleZPosition);
    G4Transform3D ModuleTP = G4Transform3D(Rotation, Position);
    new HACModule(G4Material::GetMaterial("G4_Galactic"), fLogicalVolume, ModuleTP, iModule);
  }
  
}

void HACDetector::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
