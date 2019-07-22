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
//	Creates a multi-cladding fiber
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
#include "MUV1Fiber.hh"
#include "MUV1FiberSD.hh"
#include "G4SDManager.hh"


/// \class MUV1Fiber
/// \Brief
/// MUV1Fiber class.
/// \EndBrief
///
/// \Detailed
/// Constructs multi-cladding fiber. Two cladding volumes and a core volume.
/// \EndDetailed

MUV1Fiber::MUV1Fiber(G4Material* Material, G4LogicalVolume* MotherVolume,
		G4RotationMatrix * Transform, G4double FiberRadius,
		G4double FiberLength, G4ThreeVector FiberPosition, G4int iCopy) :
    NA62VComponent(Material, MotherVolume),
	fiCopy(iCopy),
	fFiberRadius(FiberRadius),
	fFiberLength(FiberLength), //Length of the fiber
	fFiberPosition(FiberPosition), //Position of the fiber
	fTransform(Transform)
{
	CreateGeometry();
}

void MUV1Fiber::ReadGeometryParameters() {

}

void MUV1Fiber::CreateGeometry() {

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	//+                        MATERIALS		                                +
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	G4Material* Cladding2Mat = G4Material::GetMaterial(	"FiberCladding2");

	G4Material* Cladding1Mat = G4Material::GetMaterial("FiberCladding");

	// Change material between fast / full simulation
	if(fMaterial->GetName()=="DummyWLSMatCore"){
		Cladding2Mat = G4Material::GetMaterial("NonScint");
		Cladding1Mat = G4Material::GetMaterial("NonScint");
	  }

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	//+                        WLS FIBER CLADDING2                              +
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	//The outer cladding of the fiber, called cladding2
	G4double FiberInnerRadius = 0 * mm;

	//TODO: see the "t o d o" comment in MUV1Scintillato.cc from 07.11.2017 and comments in MUV1Groove.cc about the same variable
	G4double tolerance = 0.001 * mm;

	G4Tubs* cladding2_tube = new G4Tubs("cladding2_tube",
										FiberInnerRadius,
										fFiberRadius,
										0.5 * fFiberLength,
										0. * deg,
										360. * deg);

	G4LogicalVolume* cladding2_log = new G4LogicalVolume(cladding2_tube,
														 Cladding2Mat,
														 "MUV1FiberCladding2");

	new G4PVPlacement(	fTransform,
						fFiberPosition,
						cladding2_log,
						"MUV1FiberCladding2PV",
						fMotherVolume,
						false,
						0);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	//+                        WLS FIBER CLADDING                               +
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// The inner cladding , called cladding
	G4Tubs* cladding_tube = new G4Tubs("cladding_tube",
										FiberInnerRadius,
										fFiberRadius * 0.99,
										0.5 * fFiberLength-tolerance, //see the comments about the similar tolerance variable in MUV1Groove.cc
										0. * deg,
										360. * deg);

	G4LogicalVolume* cladding_log = new G4LogicalVolume(cladding_tube,
														Cladding1Mat,
														"MUV1FiberCladding");

	new G4PVPlacement(	0,
						G4ThreeVector(0., 0., 0.),
						cladding_log,
						"MUV1FiberCladdingPV",
						cladding2_log,
						false,
						0);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	//+                               WLS FIBER                                 +
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	// The core material of the WLS Fiber

	G4Tubs* fiber_tube = new G4Tubs("fiber_tube",
									FiberInnerRadius,
									fFiberRadius * 0.97,
									0.5 * fFiberLength-2*tolerance,//see the comments about the similar tolerance variable in MUV1Groove.cc
									0. * deg,
									360. * deg);

	G4LogicalVolume* fiber_log = new G4LogicalVolume(fiber_tube,
													 fMaterial,
													 "MUV1Fiber");

	fFiber_phys = new G4PVPlacement(0, // no rotation
									G4ThreeVector(0., 0., 0.), // translation position
									fiber_log, // its logical volume
									"MUV1FiberPV", // its name
									cladding_log, // its mother volume
									false, // no boolean operations
									0);

	G4SDManager* SDman = G4SDManager::GetSDMpointer();
	G4String MUV1SDname = "/MUV1/Fiber";
	G4VSensitiveDetector * Muv1FiberSD = SDman->FindSensitiveDetector(MUV1SDname);
	fiber_log->SetSensitiveDetector(Muv1FiberSD);


	//	//Set visualization properties
	//	G4VisAttributes* VisAttCladding = new G4VisAttributes(G4Colour(1.0, 0.0, 0.0));
	//	G4VisAttributes* VisAttFiber    = new G4VisAttributes(G4Colour(0.0, 1.0, 0.0));
	//	VisAttCladding -> SetVisibility(false);
	//	VisAttFiber -> SetVisibility(false);
	//	cladding_log->SetVisAttributes(VisAttCladding);
	//	fiber_log ->SetVisAttributes(VisAttFiber);

	fLogicalVolume = cladding2_log;
}

void MUV1Fiber::SetProperties() {
	//Set visualization properties
	 fVisAtt= new G4VisAttributes(G4Colour(0.0, 1.0, 0.0, 0.0));
	 fVisAtt -> SetVisibility(true);
	 fLogicalVolume ->SetVisAttributes(fVisAtt);
}
