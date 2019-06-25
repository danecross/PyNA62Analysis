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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4RotationMatrix.hh"

#include "G4SDManager.hh"

#include "CHANTIGeometryParameters.hh"
#include "CHANTIMaterialParameters.hh"

#include "CHANTIVessel.hh"
#include "CHANTIStation.hh"
#include "CHANTIRing.hh"

#include "CHANTISD.hh"
#include "G4SubtractionSolid.hh"
#include "G4UnionSolid.hh"
//
#include "CHANTISDSiPM.hh"

CHANTIVessel::CHANTIVessel(G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume)
{
  fLogicalVolume = MotherVolume;
  ReadGeometryParameters();
  // Mandatory here to Find or Build the needed materials
  CHANTIMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

CHANTIVessel::~CHANTIVessel(){}

void CHANTIVessel::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  CHANTIGeometryParameters* GeoPars = CHANTIGeometryParameters::GetInstance(); 
  
  fVesselWallThickness = GeoPars->GetCHANTIVesselWallThickness()/2; 
  fVesselWallHeight = GeoPars->GetCHANTIVesselWallHeight()/2; 
  fZVesselWall1 = GeoPars->GetCHANTIVesselWall1()/2; 
  fZVesselWall2 = GeoPars->GetCHANTIVesselWall2()/2;
  fZVesselWall3 = GeoPars->GetCHANTIVesselWall3()/2;
  fVesselPatchLength = GeoPars->GetCHANTIVesselPatchLength()/2;
  fVesselWidth = GeoPars->GetCHANTIVesselWidth()/2;
  fVesselXHole = GeoPars->GetCHANTIXInnerHoleLength()/2;
  fVesselYHole = GeoPars->GetCHANTIYInnerHoleLength()/2;
  fVesselMaterial = G4Material::GetMaterial("G4_Al");
  fVesselMaterialHole = G4Material::GetMaterial("G4_Galactic");
}

void CHANTIVessel::CreateGeometry()
{

  //
  //
  //CHANTI Vessel
  //
  //Lateral Wall
  //

  G4VSolid* VesselWall1 = new G4Box("CHANTIVesselWall1",
				    fVesselWallThickness,
				    fVesselWallHeight,
				    fZVesselWall1);
  //
  G4VSolid* VesselWall2 = new G4Box("CHANTIVesselWall2",
				    fVesselWallThickness,
				    fVesselWallHeight,
				    fZVesselWall2);
  //
  G4VSolid* VesselWall3 = new G4Box("CHANTIVesselWall3",
				    fVesselWallThickness,
				    fVesselWallHeight,
				    fZVesselWall1-fZVesselWall2);
  //
  G4VSolid* VesselPatch = new G4Box("CHANTIVesselPatch",                         
				    fVesselPatchLength,
				    fVesselWallHeight,
				    fVesselWallThickness);
  //
  // Covers and Plugs
  //

  G4VSolid* VesselCover1 = new G4Box("CHANTIVesselCover1",
				    fVesselWidth,
				    fVesselWallThickness,
				    fZVesselWall2);
  //
  G4VSolid* VesselCover2 = new G4Box("CHANTIVesselCover2",
				    fVesselWidth-fVesselPatchLength,
				    fVesselWallThickness,
				    fZVesselWall1 - fZVesselWall2);
  //
  G4VSolid* VesselPlug1 = new G4Box("CHANTIVesselPlug1",
				    fVesselWidth-fVesselWallThickness,
				    fVesselWallHeight,
				    fVesselWallThickness);
  //
  G4VSolid* VesselPlug2 = new G4Box("CHANTIVesselPlug2",
				    fVesselXHole,
				    fVesselYHole,
				    fVesselWallThickness+1*mm);
  //
  //  G4VSolid* VesselPlug = new G4SubtractionSolid("VesselPlug",VesselPlug1,VesselPlug2);
  //
  //Walls
  //
  G4LogicalVolume * AlVesselWall1 = new G4LogicalVolume(VesselWall1,
						     fVesselMaterial,
						    "CHANTIAlVesselWall1",
						    0,0,0);
  G4LogicalVolume * AlVesselWall2 = new G4LogicalVolume(VesselWall2,
						     fVesselMaterial,
						    "CHANTIAlVesselWall2",
						    0,0,0);
  G4LogicalVolume * AlVesselWall3 = new G4LogicalVolume(VesselWall3,
						     fVesselMaterial,
						    "CHANTIAlVesselWall3",
						    0,0,0);
  G4LogicalVolume * AlVesselPatch = new G4LogicalVolume(VesselPatch,
						     fVesselMaterial,
						    "CHANTIAlVesselPatch",
						    0,0,0);
  //
  // Covers and Plugs
  //
  G4LogicalVolume * AlVesselCover1 = new G4LogicalVolume(VesselCover1,
						     fVesselMaterial,
						    "CHANTIAlVesselWall1",
						    0,0,0);
  G4LogicalVolume * AlVesselCover2 = new G4LogicalVolume(VesselCover2,
						     fVesselMaterial,
						    "CHANTIAlVesselWall2",
						    0,0,0);
  G4LogicalVolume * AlVesselPlug1 = new G4LogicalVolume(VesselPlug1,
						     fVesselMaterial,
						    "CHANTIAlVesselWall3",
						    0,0,0);
  G4LogicalVolume * AlVesselPlug2 = new G4LogicalVolume(VesselPlug2,
						     fVesselMaterialHole,
						    "CHANTIAlVesselWall4",
						    0,0,0);

  //
  //Wall Placements
  //
  new G4PVPlacement(0,
		    G4ThreeVector(-1,0,0)*(fVesselWidth - fVesselWallThickness),
		    AlVesselWall1,
		    "CHANTIWall1",
		    fLogicalVolume,
		    false,
		    0);
  new G4PVPlacement(0,
		    G4ThreeVector(1,0,0)*(fVesselWidth - fVesselWallThickness) +
		    G4ThreeVector(0,0,1)*(fZVesselWall1-fZVesselWall2),
		    AlVesselWall2,
		    "CHANTIWall2",
		    fLogicalVolume,
		    false,
		    0);
  new G4PVPlacement(0,
		    G4ThreeVector(1,0,0)*(fVesselWidth - 2.0*fVesselPatchLength) +
		    G4ThreeVector(0,0,-1)*fZVesselWall2,
		    AlVesselWall3,
		    "CHANTIWall3",
		    fLogicalVolume,
		    false,
		    0);
  new G4PVPlacement(0,
		    G4ThreeVector(1,0,0)*(fVesselWidth - fVesselPatchLength - fVesselWallThickness) +
		    G4ThreeVector(0,0,-1)*(fZVesselWall1 - 2.0*fZVesselWall3),
		    AlVesselPatch,
		    "CHANTIPatch",
		    fLogicalVolume,
		    false,
		    0);

  //
  //Wall Placements
  //
  new G4PVPlacement(0,
		    G4ThreeVector(0,1,0)*(fVesselWallHeight+fVesselWallThickness)+
		    G4ThreeVector(0,0,1)*(fZVesselWall1-fZVesselWall2),
		    AlVesselCover1,
		    "CHANTICover1",
		    fLogicalVolume,
		    false,
		    0);
  //
  new G4PVPlacement(0,
		    G4ThreeVector(-1,0,0)*fVesselPatchLength+
		    G4ThreeVector(0,1,0)*(fVesselWallHeight+fVesselWallThickness)+
		    G4ThreeVector(0,0,-1)*fZVesselWall2,
		    AlVesselCover2,
		    "CHANTICover2",
		    fLogicalVolume,
		    false,
		    0);

  new G4PVPlacement(0,
		    G4ThreeVector(0,-1,0)*(fVesselWallHeight+fVesselWallThickness)+
		    G4ThreeVector(0,0,1)*(fZVesselWall1-fZVesselWall2),
		    AlVesselCover1,
		    "CHANTICover3",
		    fLogicalVolume,
		    false,
		    0);

  //
  new G4PVPlacement(0,
		    G4ThreeVector(-1,0,0)*fVesselPatchLength+
		    G4ThreeVector(0,-1,0)*(fVesselWallHeight+fVesselWallThickness)+
		    G4ThreeVector(0,0,-1)*fZVesselWall2,
		    AlVesselCover2,
		    "CHANTICover4",
		    fLogicalVolume,
		    false,
		    0);
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZVesselWall1 - fVesselWallThickness),
		    AlVesselPlug1,
		    "CHANTIPlug1",
		    fLogicalVolume,
		    false,
		    0);
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZVesselWall1 - fVesselWallThickness),
		    AlVesselPlug2,
		    "CHANTIPlug2",
		    fLogicalVolume,
		    false,
		    0);
}

void CHANTIVessel::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
