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

#include "G4Box.hh"

#include "G4LogicalVolume.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "GigaTrackerSensor.hh"
#include "G4SDManager.hh"

GigaTrackerSensor::GigaTrackerSensor(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position, G4int iCopy) : 
  NA62VComponent(Material, MotherVolume),
  fPosition(Position),
  fiCopy(iCopy)
{
  ReadGeometryParameters();
  GigaTrackerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();

}

GigaTrackerSensor::~GigaTrackerSensor() {}

void GigaTrackerSensor::ReadGeometryParameters()
{
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance();
  fActiveSensorXLength = GeoPars->GetGigaTrackerActiveSensorXLength(0);
  fActiveSensorYLength = GeoPars->GetGigaTrackerActiveSensorYLength(0);
  fActiveSensorZLength = GeoPars->GetGigaTrackerActiveSensorZLength(0);
  
  fSensorXLength = GeoPars->GetGigaTrackerSensorXLength(0);
  fSensorYLength = GeoPars->GetGigaTrackerSensorYLength(0);
  fSensorZLength = GeoPars->GetGigaTrackerSensorZLength(0);

  fNumberOfPixels = GeoPars->GetGigaTrackerNumberOfPixels();
  fGigaTrackerSensitiveDetectorName = GeoPars->GetGigaTrackerSensitiveDetectorName();

}

void GigaTrackerSensor::CreateGeometry()
{

  /////////////////////
  // Sensor envelope //
  /////////////////////

  fSolidVolume = new G4Box("GigaTrackerSensor",0.5*fSensorXLength,0.5*fSensorYLength,0.5*fSensorZLength);
  fLogicalVolume = new G4LogicalVolume(fSolidVolume, 
				       fMaterial,
				       "GigaTrackerSensor",
				       0,
				       0,
				       0);

  fPhysicalVolume = new G4PVPlacement(0,
				      fPosition,
				      fLogicalVolume,
				      "GigaTrackerSensor",
				      fMotherVolume,
				      false,
				      fiCopy);



  // Passive area (borders), 2 types 
  // - A: top and bottom
  // - B: left and right
		
  G4double borderAYLength = (fSensorYLength - fActiveSensorYLength)/2;
  G4double borderBXLength = (fSensorXLength - fActiveSensorXLength)/2;
  
  fBorderASolidVolume = new G4Box("GigaTrackerSensorBorderA",0.5*fActiveSensorXLength,0.5*borderAYLength,0.5*fSensorZLength);
  
  fBorderALogicalVolume = new G4LogicalVolume(fBorderASolidVolume,
					      G4Material::GetMaterial("G4_Si"),
					      "GigaTrackerSensorBorderA",
					      0,
					      0,        
					      0);
  
  fBorderTopPhysicalVolume = new G4PVPlacement(0,
					       G4ThreeVector(0,0.5*(fActiveSensorYLength+borderAYLength),0), 
					       fBorderALogicalVolume,
					       "GigaTrackerSensorTopBorder",
					       fLogicalVolume,
					       false,
					       0);
    
  fBorderBottomPhysicalVolume = new G4PVPlacement(0,
						  G4ThreeVector(0,-0.5*(fActiveSensorYLength+borderAYLength),0),
						  fBorderALogicalVolume,
						  "GigaTrackerSensorBottomBorder",
						  fLogicalVolume,
						  false,
						  0);
  
  //----
  
  fBorderBSolidVolume = new G4Box("GigaTrackerSensorBorderB",0.5*borderBXLength,0.5*fSensorYLength,0.5*fSensorZLength);
  
  fBorderBLogicalVolume = new G4LogicalVolume(fBorderBSolidVolume,
					      G4Material::GetMaterial("G4_Si"),
					      "GigaTrackerSensorBorderB",
					      0,
					      0,        
					      0);
    
  fBorderLeftPhysicalVolume = new G4PVPlacement(0,
						G4ThreeVector(-0.5*(fActiveSensorXLength+borderBXLength),0,0), 
						fBorderBLogicalVolume,
						"GigaTrackerSensorLeftBorder",
						fLogicalVolume,
						false,
						0);
    
  fBorderRightPhysicalVolume = new G4PVPlacement(0,
						 G4ThreeVector(0.5*(fActiveSensorXLength+borderBXLength),0,0), 
						 fBorderBLogicalVolume,
						 "GigaTrackerSensorRightBorder",
						 fLogicalVolume,
						 false,
						 0);


  // Active area (pixels) envelope (G4VParameterised must be the only daughter of a volume)
  fActiveAreaSolidVolume = new G4Box("GigaTrackerSensorActiveArea",0.5*fActiveSensorXLength,0.5*fActiveSensorYLength,0.5*fActiveSensorZLength); 
  
  fActiveAreaLogicalVolume = new G4LogicalVolume(fActiveAreaSolidVolume,
						 G4Material::GetMaterial("G4_Galactic"),
						 "GigaTrackerSensorActiveArea",
						 0,
						 0,        
						 0);
  
  fActiveAreaPhysicalVolume = new G4PVPlacement(0,
						G4ThreeVector(0,0,0), // At the center of the sensor envelope
						fActiveAreaLogicalVolume,
						"GigaTrackerSensorActiveArea",
						fLogicalVolume,
						false,
						0);
  
  ///////////
  // Pixel //
  ///////////

  fPixelParameterisation = new GigaTrackerPixelParameterisation();
  fPixelSolidVolume = new G4Box("GigaTrackerPixel",300*um,300*um,200*um); // /!\ Real size is defined by the parameterisation
  fPixelLogicalVolume = new G4LogicalVolume(fPixelSolidVolume,
					    G4Material::GetMaterial("G4_Si"),
					    "GigaTrackerPixel",
					    0,
					    0,        
					    0);
  
  ////////////////////////
  // Sensitive detector //
  ////////////////////////
  
  G4SDManager * SDMan = G4SDManager::GetSDMpointer();
  G4VSensitiveDetector * GTKSD = SDMan->FindSensitiveDetector(fGigaTrackerSensitiveDetectorName);
  fPixelLogicalVolume->SetSensitiveDetector(GTKSD);
  
  
  fPhysicalVolume = new G4PVParameterised("GigaTrackerPixels",
					  fPixelLogicalVolume,        // logical volume
					  fActiveAreaLogicalVolume,   // mother volume
					  kUndefined,                 // axis
					  fNumberOfPixels,            // replicas
					  fPixelParameterisation);    //G4VPVParameterisation
}

void GigaTrackerSensor::SetProperties()
{
  
  fLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible);
  fActiveAreaLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible);
  
  fPixelVisAtt = new G4VisAttributes(G4Colour(1.0,0.0,0.0)); //Red (red,green,blue)
  fBorderVisAtt = new G4VisAttributes(G4Colour(0.5,0.0,0.0)); //Drak Red (red,green,blue)
  
  fPixelVisAtt->SetVisibility(true);
  fBorderVisAtt->SetVisibility(true);
  
  fPixelLogicalVolume->SetVisAttributes(fPixelVisAtt);
  fBorderALogicalVolume->SetVisAttributes(fBorderVisAtt);
  fBorderBLogicalVolume->SetVisAttributes(fBorderVisAtt);
}
