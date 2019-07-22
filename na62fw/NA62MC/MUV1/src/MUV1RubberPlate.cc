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
//Copy from MUVIronPlate
//Changes to MUV1 by ykohl in March 2010
//
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

//#include "G4UnionSolid.hh"
//#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV1GeometryParameters.hh"
#include "MUV1MaterialParameters.hh"

#include "MUV1RubberPlate.hh"


MUV1RubberPlate::MUV1RubberPlate(G4Material * Material, 
				 G4LogicalVolume * MotherVolume,
				 G4ThreeVector RubberPlateSize,
				 G4ThreeVector RubberPlatePosition,
				 G4int iCopy) : NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();

  fiCopy = iCopy;

  fRubberPlateSize = RubberPlateSize;
  fRubberPlatePosition = RubberPlatePosition;

  // Mandatory here to Find or Build the needed materials
  MUV1MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

MUV1RubberPlate::~MUV1RubberPlate(){}

void MUV1RubberPlate::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();

  fHoleDimeter 			  = GeoPars->GetHoleDiameter(); // MUV1 hole dimeter + 4 mm
  fOuterSpacerOuterRadius = GeoPars->GetOuterSpacerOuterRadius();
  fBoltPositionX = GeoPars->GetBoltPositionX();
  fBoltPositionY = GeoPars->GetBoltPositionY();

}

void MUV1RubberPlate::CreateGeometry()
{
  // Build one or more boxes that will contain all the 
  // detector sections, up to fill the responsibility region

  // Define Rubber plate volume (a box)
  fSolidVolume= new G4Box("MUV1RubberPlate",
			  fRubberPlateSize.x(),
			  fRubberPlateSize.y(),
			  fRubberPlateSize.z());

  // Define Beam Pipe Hole volume (a box); enlarge by 1.01 to see a clean hole in Vis
  G4Box * BeamPipeHole = new G4Box("MUV1BeamPipeHole",
				     fHoleDimeter/2. + 5*mm,
				     fHoleDimeter/2. + 5*mm,
					 1.01*fRubberPlateSize.z());

  // Holes for connection rods. The radius of the holes is more than radius of spacers on the rods
  G4Tubs * ConnectionHole = new G4Tubs("MUV1ConnectionHole",
				     0.,
				     fOuterSpacerOuterRadius + 5*mm,
					 1.01*fRubberPlateSize.z(),
				     0.*deg,
				     360.*deg);


  G4VSolid * RubberPlateConnectionHoleLeftUp = new G4SubtractionSolid("MUV1RubberPlateConnectionHoleLeftUp",
  							fSolidVolume,
  							ConnectionHole,
                              0,
                            G4ThreeVector(-fBoltPositionX,fBoltPositionY,0.));

  G4VSolid * RubberPlateConnectionHoleRightUp = new G4SubtractionSolid("MUV1RubberPlateConnectionHoleRightUp",
		  	  	  	  	  	RubberPlateConnectionHoleLeftUp,
  							ConnectionHole,
                              0,
                            G4ThreeVector(fBoltPositionX,fBoltPositionY,0.));

  G4VSolid * RubberPlateConnectionHoleRightDown = new G4SubtractionSolid("MUV1RubberPlateConnectionHoleRightDown",
		  	  	  	  	    RubberPlateConnectionHoleRightUp,
  							ConnectionHole,
                              0,
                            G4ThreeVector(fBoltPositionX,-fBoltPositionY,0.));

  G4VSolid * RubberPlateConnectionHoleLeftDown = new G4SubtractionSolid("MUV1RubberPlateConnectionHoleLeftDown",
		  	  	  	  	  	RubberPlateConnectionHoleRightDown,
  							ConnectionHole,
                              0,
                            G4ThreeVector(-fBoltPositionX,-fBoltPositionY,0.));

  // Subtract the Pipe Hole from the Rubber plate volume
  G4VSolid * RubberPlateWithHole = new G4SubtractionSolid("MUV1RubberPlateWithHole",
		  	  	  	  	  	RubberPlateConnectionHoleLeftDown,
							BeamPipeHole,
							0,
                            G4ThreeVector(0.,0.,0.));

  // Rubber plate logical volume
  fLogicalVolume= new G4LogicalVolume(RubberPlateWithHole,           // solid
				      fMaterial,                   // material
				      "MUV1RubberPlate",              // name
				      0,                           // field manager 
				      0,                           // sensitive detector
				      0);                          // user limits

  // Rubber plate physical volume
  fPhysicalVolume = new G4PVPlacement(0,                           // no rotation
				      fRubberPlatePosition,          // position
				      fLogicalVolume,              // its logical volume
				      "MUV1RubberPlate",              // its name
				      fMotherVolume,               // its mother  volume
				      false,                       // no boolean operations
				      fiCopy);                     // copy number
  
}

void MUV1RubberPlate::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,0.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
