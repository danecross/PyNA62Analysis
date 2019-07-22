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
#include "GigaTrackerCoolingPlate.hh"

GigaTrackerCoolingPlate::GigaTrackerCoolingPlate(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position, G4int iCopy) : 
  NA62VComponent(Material, MotherVolume),
  fiCopy(iCopy),
  fPosition(Position)
{
  ReadGeometryParameters();
  GigaTrackerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();

}

GigaTrackerCoolingPlate::~GigaTrackerCoolingPlate() {}

void GigaTrackerCoolingPlate::ReadGeometryParameters() {
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance();

  // Main bloc
  fCoolingPlateXLength = GeoPars->GetCoolingPlateXLength(0); 
  fCoolingPlateYLength = GeoPars->GetCoolingPlateYLength(0);
  fCoolingPlateZLength = GeoPars->GetCoolingPlateZLength(fiCopy); 

  // Top plate
  fTopShoulderXLength = GeoPars->GetCoolingPlateTopShoulderXLength();
  fTopHollowXLength = GeoPars->GetCoolingPlateTopHollowXLength();
  fTopShoulderYLength = GeoPars->GetCoolingPlateTopShoulderYLength();
  fTopHollowYLength = GeoPars->GetCoolingPlateTopHollowYLength();
  fTopDepth = GeoPars->GetCoolingPlateTopDepth(fiCopy);

  // Bottom plate
  fBottomShoulderXLength = GeoPars->GetCoolingPlateBottomShoulderXLength();
  fBottomHollowXLength = GeoPars->GetCoolingPlateBottomHollowXLength();
  fBottomShoulderYLength = GeoPars->GetCoolingPlateBottomShoulderYLength();
  fBottomHollowYLength = GeoPars->GetCoolingPlateBottomHollowYLength();
  fBottomDepth = GeoPars->GetCoolingPlateBottomDepth(fiCopy);

  //
  fChannelsXOffset = GeoPars->GetCoolingPlateChannelsXOffset();
  fChannelsDepth = GeoPars->GetCoolingPlateChannelsDepth();

  // Envelope
  fChannelsEnvelopeXLength = GeoPars->GetCoolingPlateChannelsEnvelopeXLength();
  fChannelsEnvelopeYLength = GeoPars->GetCoolingPlateChannelsEnvelopeYLength();
  fChannelsEnvelopeZLength = GeoPars->GetCoolingPlateChannelsEnvelopeZLength();

  // Channels
  fChannelCoolantXLength = GeoPars->GetCoolingPlateChannelCoolantXLength();
  fChannelCoolantYLength = fChannelsEnvelopeYLength;
  fChannelCoolantZLength = fChannelsEnvelopeZLength;
  
  // Warning *half* wall
  fChannelHalfWallXLength = GeoPars->GetCoolingPlateChannelHalfWallXLength();
  fChannelHalfWallYLength = fChannelsEnvelopeYLength;
  fChannelHalfWallZLength = fChannelsEnvelopeZLength;

  fChannelXLength = 2*fChannelHalfWallXLength+fChannelCoolantXLength;
  fChannelYLength = fChannelsEnvelopeYLength;
  fChannelZLength = fChannelsEnvelopeZLength;
}

void GigaTrackerCoolingPlate::CreateGeometry()
{

  //
  // "Negative" footprint: these two solids will be removed from the main bloc
  //

  //Top Hollow Footprint
  fTopHollow = new G4Trd("GigaTrackerCoolingPlateTopHollow",
			 0.5*fTopHollowXLength,    	//dx1 (-dz)
			 0.5*fTopShoulderXLength,  	//dx2 (+dz)
			 0.5*fTopHollowYLength,    	//dy1 (-dz)
			 0.5*fTopShoulderYLength,	//dy2 (+dz)
			 0.5*fTopDepth);           	//dz
  //Bottom Hollow Footprint
  fBottomHollow = new G4Trd("GigaTrackerCoolingPlateBottomHollow",  
			    0.5*fBottomShoulderXLength, //dx1 (-dz)
			    0.5*fBottomHollowXLength,   //dx2 (+dz)
			    0.5*fBottomShoulderYLength, //dy1 (-dz)
			    0.5*fBottomHollowYLength,	//dy2 (+dz)
			    0.5*fBottomDepth);          //dz

  //
  // Some cooling plates lack the pools (this is encoded in the SQLite DB)
  // In that case skip the subtraction step as it triggers geometry warnings.
  // 

  fMainBloc = new G4Box("GigaTrackerCoolingMainBloc",0.5*fCoolingPlateXLength,0.5*fCoolingPlateYLength,0.5*fCoolingPlateZLength);
  fUpperWafer = new G4SubtractionSolid("GigaTrackerCoolingPlate",fMainBloc,fTopHollow,0,G4ThreeVector(0.0,0.0,0.5*(fCoolingPlateZLength - fTopDepth)));
  // Case 1. Nominal: (main bloc - upper pool) - lower pool.
  if(fTopDepth > 0 && fBottomDepth > 0) {
    fSolidVolume = new G4SubtractionSolid("GigaTrackerCoolingPlate",fUpperWafer,fBottomHollow,0,G4ThreeVector(0.0,0.0,0.5*(fBottomDepth - fCoolingPlateZLength)));
  }
  // Case 2. No top pool: (main bloc - lower pool)
  else if(fBottomDepth > 0) {
    fSolidVolume = new G4SubtractionSolid("GigaTrackerCoolingPlate",fMainBloc,fBottomHollow,0,G4ThreeVector(0.0,0.0,0.5*(fBottomDepth - fCoolingPlateZLength)));
  }
  // Case 3. No bottom pool: (main bloc - upper pool)
  else if(fTopDepth > 0) {
    fSolidVolume = new G4SubtractionSolid("GigaTrackerCoolingPlate",fMainBloc,fTopHollow,0,G4ThreeVector(0.0,0.0,0.5*(fCoolingPlateZLength - fTopDepth)));
  }
  // Case 4. No pools: (main bloc)
  else {
    fSolidVolume = fMainBloc;
  }
  fLogicalVolume = new G4LogicalVolume(fSolidVolume,fMaterial,"GigaTrackerCoolingPlate",0,0,0);
  fPhysicalVolume = new G4PVPlacement(0,
				      fPosition,
				      fLogicalVolume,
				      "GigaTrackerCoolingPlate",
				      fMotherVolume, //Mother logical volume
				      false,
				      fiCopy);


  ///////////////////////
  // Channels envelope //
  ///////////////////////
 
  G4double channelsEnvelopeZ = 0.5*(fBottomDepth - fTopDepth);
  fChannelsEnvelopeSolidVolume = new G4Box("GigaTrackerCoolingChannelsEnvelope",0.5*fChannelsEnvelopeXLength,0.5*fChannelsEnvelopeYLength,0.5*fChannelsEnvelopeZLength); 
  
  fChannelsEnvelopeLogicalVolume = new G4LogicalVolume(fChannelsEnvelopeSolidVolume,G4Material::GetMaterial("G4_Galactic"),"GigaTrackerCoolingChannelsEnvelope",0,0,0);
    
  fChannelsEnvelopePhysicalVolume = new G4PVPlacement(0,
						      G4ThreeVector(0,0,channelsEnvelopeZ),
						      fChannelsEnvelopeLogicalVolume,
						      "GigaTrackerCoolingChannelsEnvelope",
						      fLogicalVolume, //Mother logical volume
						      false,
						      0);
  
  ///////////////////
  // Channel Model //
  //  ww|cccc|ww   //
  ///////////////////
  
  fChannelSolidVolume = new G4Box("GigaTrackerCoolingChannel",0.5*fChannelXLength,0.5*fChannelYLength,0.5*fChannelZLength); 
  fChannelLogicalVolume = new G4LogicalVolume(fChannelSolidVolume,G4Material::GetMaterial("G4_Galactic"),"GigaTrackerCoolingChannel",0,0,0);

  fChannelHalfWallSolidVolume = new G4Box("GigaTrackerChannelHalfWall",0.5*fChannelHalfWallXLength,0.5*fChannelHalfWallYLength,0.5*fChannelHalfWallZLength);
  fChannelHalfWallLogicalVolume = new G4LogicalVolume(fChannelHalfWallSolidVolume,G4Material::GetMaterial("G4_Si"),"GigaTrackerChannelHalfWall",0,0,0); //Si

  G4double LeftWallPos = -0.5*(fChannelCoolantXLength+fChannelHalfWallXLength);
  G4double RightWallPos =  0.5*(fChannelCoolantXLength+fChannelHalfWallXLength);
  G4double CoolantPos = 0.0;

  fChannelLeftWallPhysicalVolume = new G4PVPlacement(0,
						     G4ThreeVector(LeftWallPos,0,0),
						     fChannelHalfWallLogicalVolume,
						     "GigaTrackerChannelHalfWall",
						     fChannelLogicalVolume, 
						     false,
						     0);

  fChannelRightWallPhysicalVolume = new G4PVPlacement(0,
						      G4ThreeVector(RightWallPos,0,0),
						      fChannelHalfWallLogicalVolume,
						      "GigaTrackerChannelHalfWall",
						      fChannelLogicalVolume, 
						      false,
						      0);

  fChannelCoolantSolidVolume = new G4Box("GigaTrackerChannelCoolant",0.5*fChannelCoolantXLength,0.5*fChannelCoolantYLength,0.5*fChannelCoolantZLength);
  fChannelCollantLogicalVolume = new G4LogicalVolume(fChannelCoolantSolidVolume,G4Material::GetMaterial("GTK_Coolant"),"GigaTrackerChannelCoolant",0,0,0); //C6F14
  fChannelCollantPhysicalVolume = new G4PVPlacement(0,
						    G4ThreeVector(CoolantPos,0,0),
						    fChannelCollantLogicalVolume,
						    "GigaTrackerChannelCoolant",
						    fChannelLogicalVolume,
						    false,
						    0);

  /////////////////////////
  // Channel replication //
  /////////////////////////

  G4int nReplicas = fChannelsEnvelopeXLength/fChannelXLength;

  fChannelPhysicalVolume = new G4PVReplica("GigaTrackerCoolingChannelWalls",
					   fChannelLogicalVolume, //logical volume
					   fChannelsEnvelopeLogicalVolume, //mother logical volume
					   kXAxis,          // replication axis
					   nReplicas,       // nReplicas
					   fChannelXLength, // width,
					   0);              // offset
}

void GigaTrackerCoolingPlate::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(0.0,0.5,0.5)); // (red,green,blue)
  fVisAtt->SetVisibility(true);
  fLogicalVolume->SetVisAttributes(fVisAtt);

  fChannelLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible);
  fChannelsEnvelopeLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible);
  fChannelHalfWallLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible); 
  
  G4VisAttributes * fVisAttChannels = new G4VisAttributes(G4Colour(0.0,.5,1.0)); // (red,green,blue)
  fVisAttChannels->SetVisibility(true);
  fChannelCollantLogicalVolume->SetVisAttributes(fVisAttChannels);
}
