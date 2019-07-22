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
// (Based on GigaTrackerStation.cc)
// --------------------------------------------------------------
//
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerMaterialParameters.hh"

#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "GigaTrackerSensorAssembly.hh"

GigaTrackerSensorAssembly::GigaTrackerSensorAssembly(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position, G4int iCopy) : 
  NA62VComponent(Material, MotherVolume),
  fiCopy(iCopy),
  fPosition(Position)
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();

}

GigaTrackerSensorAssembly::~GigaTrackerSensorAssembly() {}

void GigaTrackerSensorAssembly::ReadGeometryParameters()
{
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance(); 

  fGigaTrackerSensorAssemblyXLength = GeoPars->GetGigaTrackerSensorAssemblyXLength(0);
  fGigaTrackerSensorAssemblyYLength = GeoPars->GetGigaTrackerSensorAssemblyYLength(0);
  fGigaTrackerSensorAssemblyZLength = GeoPars->GetGigaTrackerSensorAssemblyZLength(fiCopy);


  fGigaTrackerActiveSensorXLength = GeoPars->GetGigaTrackerActiveSensorXLength(0);
  fGigaTrackerSensorZLength = GeoPars->GetGigaTrackerSensorZLength(0);

  fGigaTrackerChipXLength = GeoPars->GetGigaTrackerChipXLength(0);
  fGigaTrackerChipYLength = GeoPars->GetGigaTrackerChipYLength(0);
  fGigaTrackerChipZLength = GeoPars->GetGigaTrackerChipZLength(fiCopy);
  fGigaTrackerChipXGap = GeoPars->GetGigaTrackerChipXGap();


  fGigaTrackerBumpBondingZLength = GeoPars->GetGigaTrackerBumpBondingZLength();
  fGigaTrackerGlueLayerZLength = GeoPars->GetGigaTrackerGlueLayerZLength();
}

void GigaTrackerSensorAssembly::CreateGeometry()
{
    
  /////////////////////
  // Sensor Assembly //
  /////////////////////

  fSolidVolume = new G4Box("GigaTrackerSensorAssembly",0.5*fGigaTrackerSensorAssemblyXLength,0.5*fGigaTrackerSensorAssemblyYLength,0.5*fGigaTrackerSensorAssemblyZLength);
  fLogicalVolume = new G4LogicalVolume(fSolidVolume,G4Material::GetMaterial("G4_Galactic"),"GigaTrackerSensorAssembly",0,0,0);
  fPhysicalVolume = new G4PVPlacement(0,
				      fPosition,
				      fLogicalVolume,       // its logical volume
				      "GigaTrackerSensorAssembly", // its name
				      fMotherVolume,        // its mother  volume
				      false,                // no boolean operations
				      fiCopy);              // copy number
    
  ////////////
  // Sensor //
  ////////////

  G4double sensorZ = 0.5*(fGigaTrackerGlueLayerZLength+fGigaTrackerChipZLength+fGigaTrackerBumpBondingZLength);
  fGigaTrackerSensor = new GigaTrackerSensor(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume,G4ThreeVector(0,0,sensorZ),0);
    
  ////////////////
  // Bump bonds //
  ////////////////

  G4double bumpBondsZ = 0.5*(fGigaTrackerGlueLayerZLength+fGigaTrackerChipZLength-fGigaTrackerSensorZLength);
  fGigaTrackerBumpBonds = new GigaTrackerBumpBonds(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume,G4ThreeVector(0,0,bumpBondsZ),0);
    
  ///////////////////
  // Readout chips //
  ///////////////////

  G4double chipZ = -0.5*(fGigaTrackerBumpBondingZLength+fGigaTrackerSensorZLength-fGigaTrackerGlueLayerZLength);

  //No gap between chip along Y axis
  
  //(Avoid overlap)
  G4double chipYTop = 0.5*fGigaTrackerChipYLength+0.1*um;
  G4double chipYBottom = -0.5*fGigaTrackerChipYLength-0.1*um;

  G4int i;
  for(i=0;i<5;i++) {
    G4double chipX =  -0.5*fGigaTrackerActiveSensorXLength + 0.5*fGigaTrackerChipXLength+i*(fGigaTrackerChipXLength+2*fGigaTrackerChipXGap);
    fGigaTrackerChips.push_back(new GigaTrackerChip(G4Material::GetMaterial("G4_Si"),fLogicalVolume,G4ThreeVector(chipX,chipYTop,chipZ),i,fiCopy));
  }

  for(i=0;i<5;i++) {
    G4double chipX =  -0.5*fGigaTrackerActiveSensorXLength + 0.5*fGigaTrackerChipXLength+i*(fGigaTrackerChipXLength+2*fGigaTrackerChipXGap);
    fGigaTrackerChips.push_back(new GigaTrackerChip(G4Material::GetMaterial("G4_Si"),fLogicalVolume,G4ThreeVector(chipX,chipYBottom,chipZ),i+5,fiCopy));
  }
	
  //////////
  // Glue //
  //////////
	
  G4double GlueZ = -0.5*(fGigaTrackerChipZLength + fGigaTrackerBumpBondingZLength + fGigaTrackerSensorZLength); 
   
  fGlueSolidVolume = new G4Box("GigaTrackerGlueLayer",0.5*fGigaTrackerSensorAssemblyXLength,0.5*fGigaTrackerSensorAssemblyYLength,0.5*fGigaTrackerGlueLayerZLength);
  fGlueLogicalVolume = new G4LogicalVolume(fGlueSolidVolume,G4Material::GetMaterial("GTK_Glue"),"GigaTrackerGlueLayer",0,0,0);
  fGluePhysicalVolume = fPhysicalVolume = new G4PVPlacement(0,
							    G4ThreeVector(0.0,0.0,GlueZ),
							    fGlueLogicalVolume,       // its logical volume
							    "GigaTrackerGlueLayer", // its name
							    fLogicalVolume,        // its mother  volume
							    false,                // no boolean operations
							    fiCopy);              // copy number
	
}

void GigaTrackerSensorAssembly::SetProperties()
{
  fLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible);
  G4VisAttributes * fVisAttGlueLayer = new G4VisAttributes(G4Colour(1.0,0.0,1.0));
  fVisAttGlueLayer->SetVisibility(true);
  fGlueLogicalVolume->SetVisAttributes(fVisAttGlueLayer);
}
