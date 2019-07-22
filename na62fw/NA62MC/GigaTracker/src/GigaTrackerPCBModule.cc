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
// Created by Bob Velghe (bob.velghe@cern.ch) 2012-01-09
// --------------------------------------------------------------
//
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerMaterialParameters.hh"

#include "G4LogicalVolume.hh"
#include "GigaTrackerPCBModule.hh"

GigaTrackerPCBModule::GigaTrackerPCBModule(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position, G4int iCopy) : 
  NA62VComponent(Material, MotherVolume),
  fiCopy(iCopy),
  fPosition(Position)
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

GigaTrackerPCBModule::~GigaTrackerPCBModule() {}

void GigaTrackerPCBModule::ReadGeometryParameters()
{
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance();
  fPCBXLength = GeoPars->GetGigaTrackerPCBXLength();
  fPCBYLength = GeoPars->GetGigaTrackerPCBYLength();
  fPCBZLength = GeoPars->GetGigaTrackerPCBZLength();
  fPCBHoleXOffset = GeoPars->GetGigaTrackerPCBHoleXOffset();
  fPCBHoleXLength = GeoPars->GetGigaTrackerPCBHoleXLength();
  fPCBHoleYLength = GeoPars->GetGigaTrackerPCBHoleYLength();

  fCoolingPlateZLength = GeoPars->GetCoolingPlateZLength(fiCopy);
  fSensorAssemblyZLength = GeoPars->GetGigaTrackerSensorAssemblyZLength(fiCopy);
  fCoolingPlateTopDepth = GeoPars->GetCoolingPlateTopDepth(fiCopy);

}

void GigaTrackerPCBModule::CreateGeometry()
{
  ////////////////
  // PCB module //
  ////////////////
  fSolidVolume = new G4Box("GigaTrackerPCBModule",0.5*fPCBXLength,0.5*fPCBYLength,0.5*(fPCBZLength+fCoolingPlateZLength));
  fLogicalVolume = new G4LogicalVolume(fSolidVolume,fMaterial,"GigaTrackerPCBModule",0,0,0);
  fPhysicalVolume = new G4PVPlacement(0,
				      fPosition,
				      fLogicalVolume,
				      "GigaTrackerPCBModule",
				      fMotherVolume, 
				      false,
				      fiCopy);

  G4double PCBLeftXLength = fPCBHoleXOffset;
  G4double PCBRightXLength = fPCBXLength-PCBLeftXLength-fPCBHoleXLength;
  G4double PCBTopBottomYLength = (fPCBYLength-fPCBHoleYLength)/2;
  G4double PCBZPos = 0.5*fCoolingPlateZLength;

  fPCBLeftSolidVolume = new G4Box("GigaTrackerPCBLeft",0.5*PCBLeftXLength,0.5*fPCBYLength,0.5*fPCBZLength);
  fPCBLeftLogicalVolume = new G4LogicalVolume(fPCBLeftSolidVolume,G4Material::GetMaterial("GTK_PCB"),"GigaTrackerPCBLeft",0,0,0);

  fPCBRightSolidVolume = new G4Box("GigaTrackerPCBRight",0.5*PCBRightXLength,0.5*fPCBYLength,0.5*fPCBZLength);
  fPCBRightLogicalVolume = new G4LogicalVolume(fPCBRightSolidVolume,G4Material::GetMaterial("GTK_PCB"),"GigaTrackerPCBRight",0,0,0);

  //Top and bottom pieces are identical
  fPCBTopBottomSolidVolume = new G4Box("GigaTrackerPCBTopBottom",0.5*fPCBHoleXLength,0.5*PCBTopBottomYLength,0.5*fPCBZLength);
  fPCBTopBottomLogicalVolume = new G4LogicalVolume(fPCBTopBottomSolidVolume,G4Material::GetMaterial("GTK_PCB"),"GigaTrackerPCBTopBottom",0,0,0);

  fPCBLeftPhysicalVolume = new G4PVPlacement(0,
					     G4ThreeVector(-0.5*(fPCBHoleXLength + PCBRightXLength),0,PCBZPos),
					     fPCBLeftLogicalVolume,
					     "GigaTrackerPCBLeft",
					     fLogicalVolume,
					     false,
					     0);

  fPCBRightPhysicalVolume = new G4PVPlacement(0,
					      G4ThreeVector(0.5*(PCBLeftXLength + fPCBHoleXLength),0,PCBZPos),
					      fPCBRightLogicalVolume,
					      "GigaTrackerPCBRight",
					      fLogicalVolume,
					      false,
					      0);

  fPCBTopPhysicalVolume = new G4PVPlacement(0,
					    G4ThreeVector(0.5*(PCBLeftXLength - PCBRightXLength),0.5*(fPCBYLength - PCBTopBottomYLength),PCBZPos),
					    fPCBTopBottomLogicalVolume,
					    "GigaTrackerPCBTop",
					    fLogicalVolume,
					    false,
					    0);

  fPCBBottomPhysicalVolume = new G4PVPlacement(0,
					       G4ThreeVector(0.5*(PCBLeftXLength - PCBRightXLength),0.5*(PCBTopBottomYLength - fPCBYLength),PCBZPos),
					       fPCBTopBottomLogicalVolume,
					       "GigaTrackerPCBBottom",
					       fLogicalVolume,
					       false,
					       0);

  G4double sensorPos = 0.5*(fCoolingPlateZLength - fPCBZLength + fSensorAssemblyZLength)  - fCoolingPlateTopDepth;

  fGigaTrackerSensorAssembly = new GigaTrackerSensorAssembly(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume,G4ThreeVector(0.5*(PCBLeftXLength - PCBRightXLength),0,sensorPos),fiCopy);
  fGigaTrackerCoolingPlate = new GigaTrackerCoolingPlate(G4Material::GetMaterial("G4_Si"),fLogicalVolume,G4ThreeVector(0.5*(PCBLeftXLength - PCBRightXLength),0,-0.5*fPCBZLength),fiCopy);
}

void GigaTrackerPCBModule::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(0.0,1.0,0.0)); //Green (red,green,blue)
  fVisAtt->SetVisibility(true);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
