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
// Created by Mario Vormstein (mario.vormstein@uni-mainz.de)  2011-03-01
//
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch)     2017-11-10
//
//	Creates a box as groove for scintillator - containing fibers
//
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV1GeometryParameters.hh"
#include "MUV1MaterialParameters.hh"
#include "MUV1Groove.hh"
#include "MUV1Fiber.hh"
//#include "MUV1SD.hh"
#include "G4SDManager.hh"

/// \class MUV1Groove
/// \Brief
/// MUV1Groove class.
/// \EndBrief
///
/// \Detailed
///	Creates a groove containing a multi-clading fiber
/// \EndDetailed

MUV1Groove::MUV1Groove(G4Material * Material,
		       G4LogicalVolume * MotherVolume,
		       G4double GrooveWidth,
		       G4double GrooveDepth,
		       G4double FiberRadius,		
		       G4double FiberLength,
		       G4ThreeVector FiberPosition,
		       G4int iCopy)  :
  NA62VComponent(Material,MotherVolume),
  fLogicalVolume(NULL),
  fiCopy        (iCopy),
  fGrooveWidth  (GrooveWidth),		// Width if groove in which fiber lays
  fGrooveDepth  (GrooveDepth),
  fFiberRadius  (FiberRadius),
  fFiberLength  (FiberLength),		//Length of the fiber
  fFiberPosition(FiberPosition)	//Position of the fiber
{
  ReadGeometryParameters();
  // Mandatory here to Find or Build the needed materials
  MUV1MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void MUV1Groove::ReadGeometryParameters()
{  

}

void MUV1Groove::CreateGeometry()
{

  G4RotationMatrix* g4rot = new G4RotationMatrix(CLHEP::HepRotationZ(90.*deg));
  *g4rot = g4rot->inverse();

  G4RotationMatrix* g4rotY90 = new G4RotationMatrix(CLHEP::HepRotationY(90.*deg));
  *g4rotY90 = g4rotY90->inverse();



  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //+                               WLS GROOVE                                +
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  //TODO: see the "t o d o" comment in MUV1Scintillato.cc from 07.11.2017
  G4double tolerance = 0.001 * mm;

  G4Box* fSolidVolume = new G4Box("groove_box",
				fFiberLength / 2. - tolerance, //make the groove lenght arbitrarily smaller than the corresponding scintillator length at the x - position of the groove shortest edge (relevant for the trapezoidal half strips)
				fGrooveWidth / 2.,
				fGrooveDepth / 2. - tolerance  //the same reason as for the length, groove should not have a common surface with its mother scintillator volume
				);

  // Change material between fast / full simulation
  G4Material* GrooveMat = G4Material::GetMaterial("OpticalCement");
  if(fMaterial->GetName()=="DummyWLSMatCore"){
	  GrooveMat = G4Material::GetMaterial("NonScint");
  }

  fLogicalVolume = new G4LogicalVolume(fSolidVolume,
		  	  	  	  	  	GrooveMat,
						    "MUV1Groove",
						    0,0,0);
  
  
  

  fPhysicalVolume = new G4PVPlacement(g4rot,
		    fFiberPosition,  
		    fLogicalVolume,
		    "MUV1GroovePV",
		    fMotherVolume,
		    false,
		    0);
  
  //TODO: see the "t o d o" comment in MUV1Scintillato.cc from 07.11.2017
  // Fiber placed in groove
  fFiber = new MUV1Fiber(fMaterial,		// Material
			fLogicalVolume,		// MotherVolume
			g4rotY90,			// Rotation
			fFiberRadius,    		
			fFiberLength - 4 * tolerance, //see comments above for parameters of the fSolidVolume
			G4ThreeVector(0,0,0),
			0);
  
}

void MUV1Groove::SetProperties()
{
  //Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
