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
// --------------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2008-03-11
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
//
// Copied from MUVIronPlate
// Changes to MUV1 by ykohl in March 2010
//
// Changes by Mario Vormstein (mario.vormstein@uni-mainz.de)
//
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch)  2017-11-10
//
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4UnionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV1GeometryParameters.hh"
#include "MUV1MaterialParameters.hh"

#include "MUV1IronPlate.hh"



MUV1IronPlate::MUV1IronPlate(G4Material * Material, 
			     G4LogicalVolume * MotherVolume,
			     G4ThreeVector IronPlateSize,
			     G4ThreeVector IronPlatePosition,
			     G4int iCopy) : NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();

  fiCopy = iCopy;

  fIronPlateSize = IronPlateSize;
  fIronPlatePosition = IronPlatePosition;

  // Mandatory here to Find or Build the needed materials
  MUV1MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

MUV1IronPlate::~MUV1IronPlate(){}

void MUV1IronPlate::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();

  fInnerHoleRadius = GeoPars->GetHoleInnerRadius(); // MUV1 hole inner radius
  fConnectionHoleRadius = GeoPars->GetConnectionHoleRadius();
  fBoltPositionX = GeoPars->GetBoltPositionX();
  fBoltPositionY = GeoPars->GetBoltPositionY();

}

void MUV1IronPlate::CreateGeometry()
{
  // Build one or more boxes that will contain all the 
  // detector sections, up to fill the responsibility region
  
  // Define Iron plate volume (a box)
  fSolidVolume= new G4Box("MUV1IronPlate",
			  fIronPlateSize.x(),
			  fIronPlateSize.y(),
			  fIronPlateSize.z());

  // Define Beam Pipe Hole volume (a cylinder); enlarge by 1.01 to see a clean hole in Vis
  G4Tubs * BeamPipeHole = new G4Tubs("MUV1BeamPipeHole",
				     0.,
				     fInnerHoleRadius,
				     1.01*fIronPlateSize.z(),
				     0*deg,
				     360*deg);



  G4Tubs * ConnectionHole = new G4Tubs("MUV1ConnectionHole",
				     0.,
				     fConnectionHoleRadius,
				     1.01*fIronPlateSize.z(),
				     0.*deg,
				     360.*deg);

  //Subtract the connection holes (4 times)

  G4VSolid * IronPlateConnectionHoleLeftUp = new G4SubtractionSolid("MUV1IronPlateConnectionHoleLeftUp",
  							fSolidVolume,
  							ConnectionHole,
                              0,
                            G4ThreeVector(-fBoltPositionX,fBoltPositionY,0.));

  G4VSolid * IronPlateConnectionHoleRightUp = new G4SubtractionSolid("MUV1IronPlateConnectionHoleRightUp",
		  	  	  	  	  	IronPlateConnectionHoleLeftUp,
  							ConnectionHole,
                              0,
                            G4ThreeVector(fBoltPositionX,fBoltPositionY,0.));

  G4VSolid * IronPlateConnectionHoleRightDown = new G4SubtractionSolid("MUV1IronPlateConnectionHoleRightDown",
		  	  	  	  	    IronPlateConnectionHoleRightUp,
  							ConnectionHole,
                              0,
                            G4ThreeVector(fBoltPositionX,-fBoltPositionY,0.));

  G4VSolid * IronPlateConnectionHoleLeftDown = new G4SubtractionSolid("MUV1IronPlateConnectionHoleLeftDown",
		  	  	  	  	  	IronPlateConnectionHoleRightDown,
  							ConnectionHole,
                              0,
                            G4ThreeVector(-fBoltPositionX,-fBoltPositionY,0.));

  // Subtract the bean pipe hole from the Iron plate volume
  G4VSolid * IronPlateWithHole = new G4SubtractionSolid("MUV1IronPlateWithHole",
		  	  	  	  	  	IronPlateConnectionHoleLeftDown ,
							BeamPipeHole,
                            0,
                            G4ThreeVector(0.,0.,0.));

  // Iron plate logical volume
  fLogicalVolume= new G4LogicalVolume(IronPlateWithHole,           // solid
				      fMaterial,                   // material
				      "MUV1IronPlate",              // name
				      0,                           // field manager 
				      0,                           // sensitive detector
				      0);                          // user limits

  // Iron plate physical volume
  fPhysicalVolume = new G4PVPlacement(0,                           // no rotation
				      fIronPlatePosition,          // position
				      fLogicalVolume,              // its logical volume
				      "MUV1IronPlate",              // its name
				      fMotherVolume,               // its mother  volume
				      false,                       // no boolean operations
				      fiCopy);                     // copy number
  


}

void MUV1IronPlate::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,0.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
