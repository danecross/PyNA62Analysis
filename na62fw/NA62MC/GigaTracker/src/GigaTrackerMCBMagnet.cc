// --------------------------------------------------------------
// History:
//
// 2014-03-04 Bob Velghe (bob.velghe@cern.ch)
// - Implementation of MCBV Magnets (Archomats) 
//
// --------------------------------------------------------------
//

#include "GigaTrackerMCBMagnet.hh"

GigaTrackerMCBMagnet::GigaTrackerMCBMagnet
(G4Material * Material, G4LogicalVolume * MotherVolume,
 G4ThreeVector Position, G4int iCopy, G4int Orientation, G4double FieldScaleFactor) :
  NA62VComponent(Material, MotherVolume),
  fPosition(Position),
  fiCopy(iCopy),
  fOrientation(Orientation),
  fFieldScaleFactor(FieldScaleFactor)
{
  ReadGeometryParameters();
  GigaTrackerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void GigaTrackerMCBMagnet::ReadGeometryParameters() {

  // Read all the geometrical parameters and copy them to private members
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance();

  fXLength = GeoPars->GetGigaTrackerMCBMagnetXLength();
  fYLength = GeoPars->GetGigaTrackerMCBMagnetYLength();
  fZLength = GeoPars->GetGigaTrackerMCBMagnetZLength();

  fFieldXLength = GeoPars->GetGigaTrackerMCBMagnetFieldXLength();
  fFieldYLength = GeoPars->GetGigaTrackerMCBMagnetFieldYLength();
  fFieldZLength = GeoPars->GetGigaTrackerMCBMagnetFieldZLength();

  fBaseYLength = GeoPars->GetGigaTrackerMCBMagnetBaseYLength();
  fGapXLength  = GeoPars->GetGigaTrackerMCBMagnetGapXLength();
  fSideYLength = GeoPars->GetGigaTrackerMCBMagnetSideYLength();
  fHatYLength  = GeoPars->GetGigaTrackerMCBMagnetHatYLength();
  fBeamYPos    = GeoPars->GetGigaTrackerMCBMagnetBeamYPos();

  fFieldStrength = G4ThreeVector
    (fFieldScaleFactor * GeoPars->GetGigaTrackerArchomatMagnetFieldStrength(fiCopy), 0.0, 0.0);
  fMagnetCoreMat = G4Material::GetMaterial("G4_Fe");
}

void GigaTrackerMCBMagnet::CreateGeometry() {

  // G4 volumes

  std::stringstream s;
  s << "GigaTrackerMCBMagnet" << fiCopy;
  G4String name = s.str();

  fSolidVolume= new G4Box
    (name, 0.5*fXLength,
     0.5*(fYLength+(fBeamYPos -fBaseYLength)), 0.5*fZLength);

  fLogicalVolume = new G4LogicalVolume
    (fSolidVolume,
     fMaterial,
     name,
     0,
     0,
     0);

  G4RotationMatrix * magRot = new G4RotationMatrix();

  switch (fOrientation) {
    case 1:
      magRot->rotateX(pi*rad);
      break;
    default:
     magRot->rotateX(0);
  }

  fPhysicalVolume = new G4PVPlacement
    (magRot,
     fPosition,
     fLogicalVolume,
     name,
     fMotherVolume,
     false,
     fiCopy);

  //////////
  // Base //
  //////////

   G4double baseY = -0.5*(fFieldYLength + fBaseYLength) - (fBeamYPos - fBaseYLength);
  fBaseSolidVolume = new G4Box
    (name, 0.5*fXLength, 0.5*fBaseYLength, 0.5*fZLength);
  fBaseLogicalVolume = new G4LogicalVolume
    (fBaseSolidVolume,
     fMagnetCoreMat,
     name,
     0,
     0,
     0);
  
   G4ThreeVector basePos = G4ThreeVector(0.0,baseY,0.0);

   fBasePhysicalVolume = new G4PVPlacement
     (0,
      basePos,
      fBaseLogicalVolume,
      name,
      fLogicalVolume,
      false,
      fiCopy);

  //////////
  // Side //
  //////////

  fSideSolidVolume = new G4Box
    (name, 0.25*(fXLength-fGapXLength), 0.5*fSideYLength, 0.5*fZLength);
  fSideLogicalVolume = new G4LogicalVolume
    (fSideSolidVolume,
     fMagnetCoreMat,
     name,
     0,
     0,
     0);

   G4ThreeVector leftSidePos = G4ThreeVector
     (0.5*(fXLength-0.5*(fXLength-fGapXLength)),baseY+
      0.5*(fSideYLength+fBaseYLength),0.0);
   fLeftSidePhysicalVolume = new G4PVPlacement
     (0,
      leftSidePos,
      fSideLogicalVolume,
      name,
      fLogicalVolume,
      false,
      fiCopy);

  G4ThreeVector rightSidePos = G4ThreeVector
    (-0.5*(fXLength-0.5*(fXLength-fGapXLength)),
     baseY+0.5*(fSideYLength+fBaseYLength),0.0);
  fRightSidePhysicalVolume = new G4PVPlacement
    (0,
     rightSidePos,
     fSideLogicalVolume,
     name,
     fLogicalVolume,
     false,
     fiCopy);

  /////////
  // Hat //
  /////////

  G4double HatXLength = 0.20*(fXLength-fGapXLength);
  fHatSolidVolume = new G4Box
    (name, 0.5*HatXLength, 0.5*fHatYLength ,0.5*fZLength);
  fHatLogicalVolume = new G4LogicalVolume
    (fHatSolidVolume,
     fMagnetCoreMat,
     name,
     0,
     0,
     0);

   G4ThreeVector leftHatPos = G4ThreeVector
     (0.5*(fGapXLength+HatXLength),
      baseY+fSideYLength+0.5*(fBaseYLength+fHatYLength),0.0);
  fLeftHatPhysicalVolume = new G4PVPlacement
    (0,
     leftHatPos,
     fHatLogicalVolume,
     name,
     fLogicalVolume,
     false,
     fiCopy);

  G4ThreeVector rightHatPos = G4ThreeVector
    (-0.5*(fGapXLength+HatXLength),
     baseY+fSideYLength+0.5*(fBaseYLength+fHatYLength),0.0);
  fRightHatPhysicalVolume = new G4PVPlacement
    (0,
     rightHatPos,
     fHatLogicalVolume,
     name,
     fLogicalVolume,
     false,
     fiCopy);

  ///////////
  // Field //
  ///////////

  fFieldSolidVolume = new G4Box
    (name, 0.5*fFieldXLength, 0.5*fFieldYLength, 0.5*fFieldZLength);

  fFieldLogicalVolume = new G4LogicalVolume
    (fFieldSolidVolume,
     fMaterial,
     name,
     0,
     0,
     0);                 

  fFieldPhysicalVolume = new G4PVPlacement
    (0,
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

void GigaTrackerMCBMagnet::SetProperties() {
  fVisAtt = new G4VisAttributes();
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);

  fVisAttCore = new G4VisAttributes(G4Colour(0.5, 0.5, 0.5)); 
  fVisAttCore->SetVisibility(true);
  fBaseLogicalVolume->SetVisAttributes(fVisAttCore);
  fSideLogicalVolume->SetVisAttributes(fVisAttCore);
  fHatLogicalVolume->SetVisAttributes(fVisAttCore);

  fVisAttField = new G4VisAttributes(G4Colour(0.0, 0.0, 1.0));
  fVisAttCore->SetVisibility(true);
  fFieldLogicalVolume->SetVisAttributes(fVisAttField);
}
