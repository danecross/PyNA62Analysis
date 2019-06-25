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
//Copied from MUVScintillatorSpacer
//Changes to MUV1 by ykohl in March 2010
//
//
// Modified Mario Vormstein (mario.vormstein@cern.ch)  2011-02-01
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

#include "MUV1ScintillatorSpacer.hh"




MUV1ScintillatorSpacer::MUV1ScintillatorSpacer(G4Material * Material,
			     G4LogicalVolume * MotherVolume,
			     G4ThreeVector ScintillatorSpacerPosition,
			     G4int iCopy) : NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();

  fiCopy = iCopy;

  //fScintillatorSpacerSize = ScintillatorSpacerSize;
  fScintillatorSpacerPosition = ScintillatorSpacerPosition;

  // Mandatory here to Find or Build the needed materials
  MUV1MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

MUV1ScintillatorSpacer::~MUV1ScintillatorSpacer(){}

void MUV1ScintillatorSpacer::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();

  fInnerRadiusSpacer =		GeoPars->GetHoleInnerRadius();
  fCutBoxOuterSize =		GeoPars->GetCutBoxOuterSize();
  fCutBoxMiddleOuterSize =  GeoPars->GetCutBoxMiddleOuterSize();
  fCutBoxMiddleInnerSize =	GeoPars->GetCutBoxMiddleInnerSize();
  fCutBoxInnerSize = 		GeoPars->GetCutBoxInnerSize();
  fScintillatorSpacerSize =	GeoPars->GetScintillatorSpacerSize();
  fAirGapWidth			  = GeoPars->GetAirGapWidth();
}

void MUV1ScintillatorSpacer::CreateGeometry()
{

	// Define blank shape for spacer
	G4VSolid *blankShape = new G4Box("MUV1ScintillatorSpacerBlankShape",
			  fScintillatorSpacerSize.x(),
			  fScintillatorSpacerSize.y(),
			  fScintillatorSpacerSize.z()- 0.1 * fAirGapWidth); //! width reduction value should be always kept synchronized (smaller) with the spacer box size of the scintillator layer volume

	// Define first rectangle for cutting corners of the spacer blank shape (square)
	G4Box *cutBoxOuter = new G4Box("MUV1CutboxOuter",
							fCutBoxOuterSize.x()+0.05*mm,
							fCutBoxOuterSize.y()+0.05*mm,
							fCutBoxOuterSize.z()+0.05*mm);

	// Define second rectangle for cutting corners of the spacer blank shape (square)
	G4double cutBoxMiddleOuterXYSideLength = sqrt(2.) * (fCutBoxMiddleOuterSize.x()+0.05*mm) ;
	G4Box *cutBoxMiddleOuter = new G4Box("MUV1CutBoxMiddleOuter",
							cutBoxMiddleOuterXYSideLength/2.,
							cutBoxMiddleOuterXYSideLength/2.,
							fCutBoxMiddleOuterSize.z()+0.05*mm);

	//rotation for the second cut with MUV1CutboxOuter
	G4RotationMatrix* g4rot90 = new G4RotationMatrix();
	g4rot90->rotateX(0.*deg);
	g4rot90->rotateY(0.*deg);
	g4rot90->rotateZ(90.*deg);

	//rotation for the cut with MUV1CutBoxMiddleOuter
	G4RotationMatrix* g4rot45 = new G4RotationMatrix();
	g4rot45->rotateX(0.*deg);
	g4rot45->rotateY(0.*deg);
	g4rot45->rotateZ(45.*deg);


	//Cut out the upper right corner (+X/+Y)
	G4VSolid* blankShapeUpRight1 = new G4SubtractionSolid(
						     "blankShapeUpRight1",
						     blankShape, 
						     cutBoxOuter,
						     0,
						     G4ThreeVector(fScintillatorSpacerSize.x() - fCutBoxOuterSize.x() + 0.05*mm,
						    		       fScintillatorSpacerSize.y() - fCutBoxOuterSize.y() + 0.05*mm,
										   0));

	G4VSolid* blankShapeUpRight2 = new G4SubtractionSolid(
						     "blankShapeUpRight2",
						     blankShapeUpRight1,
						     cutBoxOuter,
						     g4rot90,
						     G4ThreeVector(fScintillatorSpacerSize.x() - fCutBoxOuterSize.y() + 0.05*mm,
						    		       fScintillatorSpacerSize.y() - fCutBoxOuterSize.x() + 0.05*mm,
										   0));

	G4VSolid* blankShapeUpRight = new G4SubtractionSolid(
						     "blankShapeUpRight",
						     blankShapeUpRight2,
						     cutBoxMiddleOuter,
						     g4rot45,
						     G4ThreeVector(fScintillatorSpacerSize.x()/2. + 4.*mm + (fCutBoxMiddleOuterSize.x() + 0.05*mm),
						    		       fScintillatorSpacerSize.y()/2. + 4.*mm + (fCutBoxMiddleOuterSize.y() + 0.05*mm),
										   0));


	//Cut out the upper left corner (-X/+Y)
	G4VSolid* blankShapeUpLeft1 = new G4SubtractionSolid(
						     "blankShapeUpLeft1",
						     blankShapeUpRight,
						     cutBoxOuter,
						     0,
						     G4ThreeVector(-fScintillatorSpacerSize.x() + fCutBoxOuterSize.x() - 0.05*mm,
						    		       fScintillatorSpacerSize.y() - fCutBoxOuterSize.y() + 0.05*mm,
										   0));

	G4VSolid* blankShapeUpLeft2 = new G4SubtractionSolid(
						     "blankShapeUpLeft2",
						     blankShapeUpLeft1,
						     cutBoxOuter,
						     g4rot90,
						     G4ThreeVector(-fScintillatorSpacerSize.x() + fCutBoxOuterSize.y() - 0.05*mm,
						    		       fScintillatorSpacerSize.y() - fCutBoxOuterSize.x() + 0.05*mm,
										   0));

	G4VSolid* blankShapeUpLeft = new G4SubtractionSolid(
						     "blankShapeUpLeft",
						     blankShapeUpLeft2,
						     cutBoxMiddleOuter,
						     g4rot45,
						     G4ThreeVector(-fScintillatorSpacerSize.x()/2. - 4.*mm - (fCutBoxMiddleOuterSize.x() + 0.05*mm),
						    		       fScintillatorSpacerSize.y()/2. + 4.*mm + (fCutBoxMiddleOuterSize.y() + 0.05*mm),
										   0));


	//Cut out the bottom left corner (-X/-Y)
	G4VSolid* blankShapeBottomLeft1 = new G4SubtractionSolid(
						     "blankShapeBottomLeft1",
						     blankShapeUpLeft,
						     cutBoxOuter,
						     0,
						     G4ThreeVector(-fScintillatorSpacerSize.x() + fCutBoxOuterSize.x() - 0.05*mm,
						    		       -fScintillatorSpacerSize.y() + fCutBoxOuterSize.y() - 0.05*mm,
										   0));

	G4VSolid* blankShapeBottomLeft2 = new G4SubtractionSolid(
						     "blankShapeBottomLeft2",
						     blankShapeBottomLeft1,
						     cutBoxOuter,
						     g4rot90,
						     G4ThreeVector(-fScintillatorSpacerSize.x() + fCutBoxOuterSize.y() - 0.05*mm,
						    		       -fScintillatorSpacerSize.y() + fCutBoxOuterSize.x() - 0.05*mm,
										   0));

	G4VSolid* blankShapeBottomLeft = new G4SubtractionSolid(
						     "blankShapeBottomLeft",
						     blankShapeBottomLeft2,
						     cutBoxMiddleOuter,
						     g4rot45,
						     G4ThreeVector(-fScintillatorSpacerSize.x()/2. - 4.*mm - (fCutBoxMiddleOuterSize.x() + 0.05*mm),
						    		       -fScintillatorSpacerSize.y()/2. - 4.*mm - (fCutBoxMiddleOuterSize.y() + 0.05*mm),
										   0));


	//Cut out the bottom right corner (X/-Y)
	G4VSolid* blankShapeBottomRight1 = new G4SubtractionSolid(
						     "blankShapeBottomLeft1",
						     blankShapeBottomLeft,
						     cutBoxOuter,
						     0,
						     G4ThreeVector(fScintillatorSpacerSize.x() - fCutBoxOuterSize.x() + 0.05*mm,
						    		       -fScintillatorSpacerSize.y() + fCutBoxOuterSize.y() - 0.05*mm,
										   0));

	G4VSolid* blankShapeBottomRight2 = new G4SubtractionSolid(
						     "blankShapeBottomLeft2",
						     blankShapeBottomRight1,
						     cutBoxOuter,
						     g4rot90,
						     G4ThreeVector(fScintillatorSpacerSize.x() - fCutBoxOuterSize.y() + 0.05*mm,
						    		       -fScintillatorSpacerSize.y() + fCutBoxOuterSize.x() - 0.05*mm,
										   0));

	G4VSolid* blankShapeBottomRight = new G4SubtractionSolid(
						     "blankShapeBottomLeft",
						     blankShapeBottomRight2,
						     cutBoxMiddleOuter,
						     g4rot45,
						     G4ThreeVector(fScintillatorSpacerSize.x()/2. + 4.*mm + (fCutBoxMiddleOuterSize.x() + 0.05*mm),
						    		       -fScintillatorSpacerSize.y()/2. - 4.*mm - (fCutBoxMiddleOuterSize.y() + 0.05*mm),
										   0));


	//Define a beam pipe hole for cutting the center of the spacer
	G4Tubs * Hole = new G4Tubs("MUV1Hole",
    				     0.,
    				     fInnerRadiusSpacer,
    				     fScintillatorSpacerSize.z()*1.01,
    				     0.*deg,
    				     360.*deg);

  // Subtract the pipe hole from the spacer volume
  G4VSolid * ScintillatorSpacerWithHole = new G4SubtractionSolid("MUV1ScintillatorSpacerWithHole",
		  	  	  	  	  	  blankShapeBottomRight,
		  	  	  	  	  	  Hole,
							  0,
							  G4ThreeVector(0.,0.,0.));


  // Spacer logical volume
  fLogicalVolume= new G4LogicalVolume(ScintillatorSpacerWithHole,           // solid
				      fMaterial,                   // material
				      "MUV1ScintillatorSpacer",              // name
				      0,                           // field manager 
				      0,                           // sensitive detector
				      0);                          // user limits

  // Spacer physical volume
  fPhysicalVolume = new G4PVPlacement(0,                           // no rotation
				      fScintillatorSpacerPosition,          // position
				      fLogicalVolume,              // its logical volume
				      "MUV1ScintillatorSpacer",              // its name
				      fMotherVolume,               // its mother  volume
				      false,                       // no boolean operations
				      fiCopy);                     // copy number
  


}

void MUV1ScintillatorSpacer::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,0.0,0.0,0.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
