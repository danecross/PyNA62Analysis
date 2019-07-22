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
// Updated by Simone Schuchmann for new collimator TCX in 2018
// (2019-05-30)
//
// Modified by Paolo Massarotti 2012-03-08
// Created by Simone Bifani (Simone.Bifani@cern.ch) 2008-03-10
// --------------------------------------------------------------
//
#include "globals.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerMaterialParameters.hh"
#include "GigaTrackerCollimator.hh"

#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4GDMLParser.hh"
#include "G4SubtractionSolid.hh"
#include "G4UnionSolid.hh"

GigaTrackerCollimator::GigaTrackerCollimator(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position, G4int iCopy) : 
  NA62VComponent(Material, MotherVolume),
  fPosition(Position),
  fiCopy(iCopy),
  fHoleSolidVolume(nullptr),
  fHoleLogicalVolume(nullptr),
  fHolePhysicalVolume(nullptr)
{
  ReadGeometryParameters();
  // Mandatory here to Find or Build the needed materials
  GigaTrackerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();

}

GigaTrackerCollimator::~GigaTrackerCollimator() {}

void GigaTrackerCollimator::ReadGeometryParameters()
{

  // Read all the geometrical parameters and copy them to private members
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance();

  fOuterXLength = GeoPars->GetGigaTrackerCollimatorOuterXLength();
  fOuterYLength = GeoPars->GetGigaTrackerCollimatorOuterYLength();
  fInnerXLength = GeoPars->GetGigaTrackerCollimatorInnerXLength();
  fInnerYLength = GeoPars->GetGigaTrackerCollimatorInnerYLength();
  fZLength = GeoPars->GetGigaTrackerCollimatorZLength();
  fCollimatorDesign = GeoPars->GetGigaTrackerCollimatorDesign();
  if(fCollimatorDesign.contains("2018")) fGDML = GeoPars->GetGigaTrackerCollimatorGDMLPath();
}

void GigaTrackerCollimator::CreateGeometry()
{
  if(fCollimatorDesign.contains("2018")){
    CreateGeometry2018();
  }
  
  if(fCollimatorDesign.contains("2016") or fCollimatorDesign.contains("2017")){
    CreateGeometry20162017();
  }
  
}
void GigaTrackerCollimator::CreateGeometry2018()
{
  
  // Beam pipe and inner part
  G4RotationMatrix * rot = new G4RotationMatrix();
  rot->rotateY(90*deg);
  G4RotationMatrix * rotColl = new G4RotationMatrix();
  rotColl->rotateY(90*deg);
  rotColl->rotateX(90*deg);

  G4double inRadiusOuter = fOuterXLength*1.05;
  G4double inRadiusInner = fOuterYLength;
  G4double smallX = fInnerXLength;
  G4double smallY = fInnerYLength;

  G4ThreeVector position0 = G4ThreeVector(0.,0.,0.);
  G4ThreeVector position1 = G4ThreeVector(-0.5*smallX,0.0,0.0);
  G4ThreeVector position2 = G4ThreeVector(0.5*smallX,0.0,0.0);

  G4Tubs *solid = new G4Tubs("solid", 0,inRadiusOuter,0.5*fZLength,+0*deg, 360*deg);
  G4Tubs *solid2 = new G4Tubs("solid2", 0,inRadiusOuter,0.5*fZLength+2*mm,+0*deg, 360*deg);
  G4Box *box = new G4Box("box",0.502*smallX,0.5*smallY,0.5*fZLength+0.1*mm);
  G4VSolid* cylinder1 = new G4Tubs("Cylinder1",0.,inRadiusInner,0.5*fZLength+0.1*mm,-90.*deg,180*deg);
  G4VSolid* cylinder2 = new G4Tubs("Cylinder2",0.,inRadiusInner,0.5*fZLength+0.1*mm,90.*deg,180*deg);
  
  G4VSolid* unionBC  = new G4SubtractionSolid("unionBC", solid,cylinder1,0,position2);
  G4VSolid* beampipe = new G4SubtractionSolid("beampipe", unionBC,cylinder2,0,position1);
  fInnerPart = new G4SubtractionSolid("innerPart",beampipe,box,0,position0);

  // Collimator Rings
  G4GDMLParser *fParser = new G4GDMLParser();
  fParser->Read(fGDML);
  G4LogicalVolume *rlogic = fParser->GetVolume("PVPart__Feature0021");
  G4VSolid *rgdml = (G4VSolid*)rlogic->GetSolid();

  G4VSolid *ring1 = new G4UnionSolid("ring1",rgdml,rgdml,0,G4ThreeVector(-300.0*mm,0.0,0.0));
  G4VSolid *ring2 = new G4UnionSolid("ring2",ring1,rgdml,0,G4ThreeVector(-600.0*mm,0.0,0.0));
  G4VSolid *ring3 = new G4UnionSolid("ring3",ring2,rgdml,0,G4ThreeVector( 300.0*mm,0.0,0.0));
  G4VSolid* ringsolid = new G4SubtractionSolid("ring",ring3,solid2,rot,position0);
  
  // Collimator tube
  fLogicalVolume = new G4LogicalVolume(fInnerPart,
                                       G4Material::GetMaterial("G4_STAINLESS-STEEL"),// material
				       "GigaTrackerCollimator2018", // name
				       0,                       // field manager
				       0,                       // sensitive detector
				       0);                      // user limits
  
  fPhysicalVolume = new G4PVPlacement(0,
				      fPosition,
				      fLogicalVolume,          // its logical volume
				      "GigaTrackerCollimator2018", // its name
				      fMotherVolume,           // its mother  volume
				      false,                   // no boolean operations
				      fiCopy);                 // copy number
  
  // Collimator rings, material Fe
  fLogicalVolumeCRing = new G4LogicalVolume(ringsolid,
					    fMaterial,               // material
					    "GigaTrackerCollimatorRing2018", // name
					    0,                       // field manager
					    0,                       // sensitive detector
					    0);                      // user limits
  
  
  fPhysicalVolumeCRing = new G4PVPlacement(rotColl,
					   fPosition+G4ThreeVector(0.,0.,0.0*mm),
					   fLogicalVolumeCRing,
					   "GigaTrackerCollimatorRing2018", // its name
					   fMotherVolume,           // its mother  volume
					   false,                   // no boolean operations
					   fiCopy);                 // copy number

}
void GigaTrackerCollimator::CreateGeometry20162017(){
  //
  // G4 volumes
  //
  G4double HalfOuterXLength = 0.5 * fOuterXLength;
  G4double HalfOuterYLength = 0.5 * fOuterYLength;
  G4double HalfInnerXLength = 0.5 * fInnerXLength;
  G4double HalfInnerYLength = 0.5 * fInnerYLength;
  G4double HalfZLength = 0.5 * fZLength;

  // Create collimator

  fSolidVolume = new G4Box("GigaTrackerCollimator1", HalfOuterXLength, HalfOuterYLength, HalfZLength);
  fLogicalVolume = new G4LogicalVolume(fSolidVolume,            // solid
				       fMaterial,               // material
				       "GigaTrackerCollimator", // name
				       0,                       // field manager 
				       0,                       // sensitive detector
				       0);                      // user limits

  fPhysicalVolume = new G4PVPlacement(0,
				      fPosition +
				      G4ThreeVector(1,0,0)*(HalfInnerXLength - HalfOuterXLength)
				      +G4ThreeVector(0,-1,0)*(- HalfInnerYLength - HalfOuterYLength),
				      fLogicalVolume,          // its logical volume
				      "GigaTrackerCollimator", // its name
				      fMotherVolume,           // its mother  volume
				      false,                   // no boolean operations
				      fiCopy);                 // copy number

  fPhysicalVolume = new G4PVPlacement(0,
				      fPosition +
				      G4ThreeVector(1,0,0)*(-HalfInnerXLength - HalfOuterXLength)
				      + G4ThreeVector(0,-1,0)*(- HalfInnerYLength + HalfOuterYLength),
				      fLogicalVolume,          // its logical volume
				      "GigaTrackerCollimator", // its name
				      fMotherVolume,           // its mother  volume
				      false,                   // no boolean operations
				      fiCopy);                 // copy number
  fPhysicalVolume = new G4PVPlacement(0,
				      fPosition +
				      G4ThreeVector(1,0,0)*(-HalfInnerXLength + HalfOuterXLength)
				      + G4ThreeVector(0,-1,0)*(HalfInnerYLength + HalfOuterYLength),
				      fLogicalVolume,          // its logical volume
				      "GigaTrackerCollimator", // its name
				      fMotherVolume,           // its mother  volume
				      false,                   // no boolean operations
				      fiCopy);                 // copy number
  fPhysicalVolume = new G4PVPlacement(0,
				      fPosition +
				      G4ThreeVector(1,0,0)*(HalfInnerXLength + HalfOuterXLength)
				      + G4ThreeVector(0,-1,0)*(HalfInnerYLength - HalfOuterYLength),
				      fLogicalVolume,          // its logical volume
				      "GigaTrackerCollimator", // its name
				      fMotherVolume,           // its mother  volume
				      false,                   // no boolean operations
				      fiCopy);                 // copy number
}

void GigaTrackerCollimator::SetProperties()
{

  // Set visualization properties
  fVisAtt = new G4VisAttributes(G4Colour(0.5, 0.5, 0.5));
  fVisAtt->SetVisibility(true);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
