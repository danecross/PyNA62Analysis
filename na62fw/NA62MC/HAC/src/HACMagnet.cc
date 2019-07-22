#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "G4Box.hh"
#include "G4Tubs.hh"

#include "HACGeometryParameters.hh"
#include "HACMaterialParameters.hh"

#include "HACMagnet.hh"

HACMagnet::HACMagnet(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position) : NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();

  fPosition = Position;

  // Mandatory here to Find or Build the needed materials
  HACMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

HACMagnet::~HACMagnet(){}

void HACMagnet::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  HACGeometryParameters* GeoPars = HACGeometryParameters::GetInstance();
  fZLength = GeoPars->GetHACMagnetZLength();  
  fXLength = GeoPars->GetHACMagnetXLength();  
  fYLength = GeoPars->GetHACMagnetYLength();  
  fFieldStrength = GeoPars->GetHACMagnetFieldStrength();
}

void HACMagnet::CreateGeometry()
{
  G4double HalfZLength = 0.5*fZLength;
  fSolidVolume = new G4Box("Magnet",fXLength/2.,fYLength/2.,HalfZLength);
  fLogicalVolume= new G4LogicalVolume(fSolidVolume,fMaterial,"Magnet",0,0,0);
  fPhysicalVolume = new G4PVPlacement(0,fPosition,fLogicalVolume,"Magnet",fMotherVolume,false,0);
  fMagField = new G4UniformMagField(G4ThreeVector(0., fFieldStrength, 0.));
  fFieldMgr = new G4FieldManager(fMagField);
  fFieldMgr->SetDetectorField(fMagField);
  fFieldMgr->CreateChordFinder(fMagField);
  fLogicalVolume->SetFieldManager(fFieldMgr, true);
}

void HACMagnet::SetProperties()
{
  // Set visualization properties
 fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
 fVisAtt -> SetVisibility(false);
 fLogicalVolume ->SetVisAttributes(fVisAtt);
}
