// --------------------------------------------------------------
// History:
//
// 2014-03-04 Bob Velghe (bob.velghe@cern.ch)
// - Implementation of MDXH Magnet (TRIM5) 
//
// --------------------------------------------------------------
//

#include "GigaTrackerMDXMagnet.hh"

GigaTrackerMDXMagnet::GigaTrackerMDXMagnet
(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position, G4int iCopy, G4double FieldScaleFactor) :
  NA62VComponent(Material,MotherVolume),
  fPosition(Position),
  fiCopy(iCopy),
  fFieldScaleFactor(FieldScaleFactor)
{
  ReadGeometryParameters();
  GigaTrackerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

GigaTrackerMDXMagnet::~GigaTrackerMDXMagnet() {}

void GigaTrackerMDXMagnet::ReadGeometryParameters() {

  // Read all the geometrical parameters and copy them to private members
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance();

  fXLength = GeoPars->GetGigaTrackerMDXMagnetXLength();
  fYLength = GeoPars->GetGigaTrackerMDXMagnetYLength();
  fZLength = GeoPars->GetGigaTrackerMDXMagnetZLength();  

  fFieldXLength = GeoPars->GetGigaTrackerMDXMagnetFieldXLength();
  fFieldYLength = GeoPars->GetGigaTrackerMDXMagnetFieldYLength();
  fFieldZLength = GeoPars->GetGigaTrackerMDXMagnetFieldZLength();

  fGapXLength = GeoPars->GetGigaTrackerMDXMagnetGapXLength();
  fGapYLength = GeoPars->GetGigaTrackerMDXMagnetGapYLength();
  fGapZLength = fZLength;

  fFieldStrength = G4ThreeVector
    (0.0, fFieldScaleFactor*GeoPars->GetGigaTrackerTRIM5MagnetFieldStrength(), 0.0);

  fMagnetCoreMat = G4Material::GetMaterial("G4_Fe");
}


void GigaTrackerMDXMagnet::CreateGeometry() {

  // G4 volumes

  std::stringstream s;
  s << "GigaTrackerMDXMagnet" << fiCopy;
  G4String name = s.str();

  fSolidVolume= new G4Box(name, 0.5*fXLength, 0.5*fYLength, 0.5*fZLength);

  fLogicalVolume= new G4LogicalVolume(fSolidVolume,
				      fMaterial,         
				      name, 
				      0,                  
				      0,                  
				      0);                 

  fPhysicalVolume = new G4PVPlacement(0,
				      fPosition,
				      fLogicalVolume,     
				      name,
				      fMotherVolume,       
				      false,               
				      fiCopy);             


  //////////////////
  // Top / Bottom //
  //////////////////
  
  fTopBottomSolidVolume = new G4Box(name, 0.5*fXLength, 0.25*(fYLength - fGapYLength), 0.5*fZLength);

  fTopBottomLogicalVolume = new G4LogicalVolume(fTopBottomSolidVolume,
				      fMagnetCoreMat,         
				      name, 
				      0,                  
				      0,                  
				      0);                 


  G4ThreeVector topPos = G4ThreeVector(0.0,0.5*fYLength - 0.25*(fYLength - fGapYLength),0.0);
  fTopPhysicalVolume = new G4PVPlacement(0,
				      topPos,
				      fTopBottomLogicalVolume,     
				      name,
				      fLogicalVolume,       
				      false,               
				      fiCopy);      
				      
	G4ThreeVector bottomPos =  G4ThreeVector(0.0,-0.5*fYLength + 0.25*(fYLength - fGapYLength),0.0);
  fBottomPhysicalVolume = new G4PVPlacement(0,
				      bottomPos,
				      fTopBottomLogicalVolume,     
				      name,
				      fLogicalVolume,       
				      false,               
				      fiCopy);           
     

  ///////////
  // Sides //
  ///////////

  fSideSolidVolume = new G4Box(name,  0.25*(fXLength - fGapXLength), 0.5*fGapYLength, 0.5*fZLength);

  fSideLogicalVolume = new G4LogicalVolume(fSideSolidVolume,
				      fMagnetCoreMat,         
				      name, 
				      0,                  
				      0,                  
				      0);                 

  G4ThreeVector leftPos = G4ThreeVector(0.5*fXLength - 0.25*(fXLength - fGapXLength),0.0,0.0);
  fLeftPhysicalVolume = new G4PVPlacement(0,
				      leftPos,
				      fSideLogicalVolume,     
				      name,
				      fLogicalVolume,       
				      false,               
				      fiCopy);      
				      
	G4ThreeVector rightPos =  G4ThreeVector(-0.5*fXLength + 0.25*(fXLength - fGapXLength),0.0,0.0);
  fBottomPhysicalVolume = new G4PVPlacement(0,
				      rightPos,
				      fSideLogicalVolume,     
				      name,
				      fLogicalVolume,       
				      false,           
				      fiCopy);      
  ///////////
  // Field //
  ///////////

  fFieldSolidVolume = new G4Box(name, 0.5*fFieldXLength, 0.5*fFieldYLength, 0.5*fFieldZLength);

  fFieldLogicalVolume = new G4LogicalVolume(fFieldSolidVolume,
				      fMaterial,         
				      name, 
				      0,                  
				      0,                  
				      0);                 

  fFieldPhysicalVolume = new G4PVPlacement(0,
				      G4ThreeVector(0.0,0.0,0.0),
				      fFieldLogicalVolume,     
				      name, 
				      fLogicalVolume,       
				      false,               
				      fiCopy);      


  fMagField = new G4UniformMagField(fFieldStrength);
  fFieldMgr = new G4FieldManager(fMagField);
  fFieldMgr->SetDetectorField(fMagField);
  fFieldMgr->CreateChordFinder(fMagField);
  fFieldLogicalVolume->SetFieldManager(fFieldMgr, true);

}

void GigaTrackerMDXMagnet::SetProperties()
{
  fVisAtt = new G4VisAttributes();
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
 
  fVisAttCore = new G4VisAttributes(G4Colour(0.5, 0.5, 0.5)); 
  fVisAttCore->SetVisibility(true);
  fTopBottomLogicalVolume->SetVisAttributes(fVisAttCore);
  fSideLogicalVolume->SetVisAttributes(fVisAttCore);
  
  fVisAttField = new G4VisAttributes(G4Colour(0.0, 0.0, 1.0));
  fVisAttCore->SetVisibility(true);
  fFieldLogicalVolume->SetVisAttributes(fVisAttField);
}
