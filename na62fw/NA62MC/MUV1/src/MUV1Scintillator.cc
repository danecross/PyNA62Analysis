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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2008-03-16
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
//
//Copied from MUVScintillator from HArish
//Changes to MUV1 by ykohl in MArch 2010
//Now you can see the holes for the fibers and also the BoxCuts from ScintCut 
//
//
// Modified Mario Vormstein (mario.vormstein@cern.ch)  2011-06-27
//
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch)   2017-11-10
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

#include "MUV1Scintillator.hh"
#include "MUV1SD.hh"
#include "G4SDManager.hh"
#include "MUV1Groove.hh"
#include "MUV1Fiber.hh"

#include "G4LogicalBorderSurface.hh"
#include "G4LogicalSkinSurface.hh"
#include "G4OpticalSurface.hh"


/// \class MUV1Scintillator
/// \Brief
/// MUV1Scintillator class.
/// \EndBrief
///
/// \Detailed
/// In this class die different substructures of the MUV1 are called.
///	This are :
///	Creates a scintillator consisting of two volumes, the wrapping and core material.
///	For the outer fibers holes are cut into the wrapping
/// Into the scintillator two grooves (which contain fibers) are placed
/// \EndDetailed



MUV1Scintillator::MUV1Scintillator(G4RotationMatrix* Transform,
		G4Material * Material, G4LogicalVolume * MotherVolume,
		G4ThreeVector BareScintillatorSize, G4ThreeVector ScintillatorSize, G4ThreeVector ScintillatorPosition,
		G4bool boolOpp, G4int iCopy, G4int Logical) :
	NA62VComponent(Material, MotherVolume),
	fLogicalVolume(nullptr),
	fiCopy(iCopy),
	fBareScintillatorSize(BareScintillatorSize),
	fScintillatorSize(ScintillatorSize),
	fScintillatorPosition(ScintillatorPosition),
	fboolOpp(boolOpp),
	fLogical(Logical),
	fTransform(Transform)
{
	ReadGeometryParameters();


	// Mandatory here to Find or Build the needed materials
	MUV1MaterialParameters::GetInstance();

	CreateGeometry();
	SetProperties();
}

MUV1Scintillator::~MUV1Scintillator() {
}

void MUV1Scintillator::ReadGeometryParameters() {
	// Read all the geometrical parameters and copy them to private members
	MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();

	fScintillatorMotherSize.setX( GeoPars -> GetScintMotherWidth() );
	fScintillatorMotherSize.setY( GeoPars -> GetScintMotherLength() );
	fScintillatorMotherSize.setZ( GeoPars -> GetScintMotherThickness() );

    fScintLengthStandard    = GeoPars->GetScintLengthStandard();
    fScintLengthMiddleStd   = GeoPars->GetScintLengthMiddleStd();
    fScintLengthMiddleOuter = GeoPars->GetScintLengthMiddleOuter();
    fScintWidthStandard     = GeoPars->GetScintWidthStandard();
    fScintWidthMiddle       = GeoPars->GetScintWidthMiddle();

	fFiberOuterRadius = GeoPars -> GetFiberRadius();
	fSkinWidth = GeoPars->GetSkinWidth(); // Skin width of the aluminum layer of the scintillator
	fAirGapWidth = GeoPars->GetAirGapWidth();

	fGrooveWidth   = GeoPars->GetGrooveWidth();
	fGrooveDepth   = GeoPars->GetGrooveDepth();
	fGroovePositionInStandardScintillator = GeoPars->GetGroovePositionInStandardScintillator();
	fCutDepthShort = GeoPars->GetCutDepthShort();
	fCutDepthLong  = GeoPars->GetCutDepthLong();
	fCutWidth      = GeoPars->GetCutWidth();
}

void MUV1Scintillator::CreateGeometry() {


	// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	//
	// Scintillator mother volume. It will contain all the materials which the scintillator strips are made of: scintillator, wrapper, fibers, grooves, glue, PMTs
	//
	G4double ScintMotherWidth     =   fScintillatorMotherSize.x();
	G4double ScintMotherLength    =   fScintillatorMotherSize.y();
	G4double ScintMotherThickness =   fScintillatorMotherSize.z();

	if(fboolOpp)	{//half-size scintillator strips

		if(fLogical>0)	{//fLogical=1,2,3,4 //central trapezoidal narrow half strips
			ScintMotherWidth -= (fScintWidthStandard - fScintWidthMiddle);
			ScintMotherLength = ScintMotherLength/2. - (fScintLengthStandard/2. - fScintLengthMiddleOuter);
		}
		else if(fLogical==0)	{ //normal half strips
			ScintMotherLength /= 2.;
		}
		else if(fLogical==-1)	{ // central rectangular narrow half strips
			ScintMotherWidth -= (fScintWidthStandard - fScintWidthMiddle);
			ScintMotherLength = ScintMotherLength/2. - (fScintLengthStandard/2. - fScintLengthMiddleStd);
		}
		else	{
			G4cout <<"WARNING: wrong type of half size scintillator strip. Default scintillator mother volume will be created"<< G4endl;
		}
	}
	else if(fLogical==-2)	{ //side short strips with special shape of mother volumes

		//TODO: at the moment make the mother volume simply a little bit larger than its daughter scintillator.
		//In the future, this mother volume should be union of several shapes to avoid intersection with the connection rods and spacers
		ScintMotherLength = 2 * ( fScintillatorSize.y() + fAirGapWidth );
	}

	G4Box* scintillator_mother = new G4Box("MUV1Scintillator_mother_box", ScintMotherWidth/2., ScintMotherLength/2., ScintMotherThickness/2.);
	// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



	//This box is the outer limit of the scintillator
	G4Box* scintillator_box = new G4Box("MUV1scintillator_box",  fScintillatorSize.x(),  fScintillatorSize.y(),  fScintillatorSize.z());

	//This box is the air layer between alu and scint material
	G4Box* scintillator_box_AirGap = new G4Box("MUV1scintillator_box_AirGap", (fScintillatorSize.x()-fSkinWidth),  (fScintillatorSize.y()-fSkinWidth),  (fScintillatorSize.z()-fSkinWidth) );

	//This is the core box (scint) of the scintillator
	G4Box* scintillator_box_core = new G4Box("MUV1scintillator_box_core",  fBareScintillatorSize.x(),  fBareScintillatorSize.y(),  fBareScintillatorSize.z());

	G4LogicalVolume* scintillator_box_log    = 0;
	G4LogicalVolume* scintillator_mother_log = 0;



	// Position of the cutting box for the inner scintillators type 4
	// Holes for the fibers are placed, too
	if (fboolOpp && fLogical > 0) {

		// cutBoxTollerance parameter to avoid overlaps of the mother volumes of trapezoidal strips with the central iron spacer.
		// This parameter should be kept as large as possible (to avoid extrusion of scintillator volumes by grooves) but negative.
		// A larger value than -0.0075 mm (e.g. -0.005 mm) is not enough to prevent the overlap.
		G4double cutBoxTollerance = -0.0075 * mm ;

		// 44 mm is the depth of the triangular cut at the end of the scintillator. In other words, it's the length of the equal catheti of the triangular cut.
		// Size of the cut box needs to be modified according to the position of the box center.
		G4double cutBoxSize       = sqrt(2.) * (44 - 2. * cutBoxTollerance) * mm;

		G4RotationMatrix* g4rot45 = new G4RotationMatrix();
		g4rot45->rotateX(0.*deg);		g4rot45->rotateY(0.*deg);		g4rot45->rotateZ(45.*deg);

		G4ThreeVector cutPositionMom = G4ThreeVector(0, 0, 0);
		G4ThreeVector cutPositionBox = G4ThreeVector(0, 0, 0);
		G4ThreeVector cutPositionAir = G4ThreeVector(0, 0, 0);
		G4ThreeVector cutPositionCore= G4ThreeVector(0, 0, 0);

		cutPositionMom[0] = (fLogical == 1 || fLogical == 3 ? 1 : -1) * (ScintMotherWidth/2. + cutBoxTollerance);
		cutPositionMom[1] = (fLogical == 3 || fLogical == 4 ? 1 : -1) * (ScintMotherLength/2.+ cutBoxTollerance);

		cutPositionBox[0] = (fLogical == 1 || fLogical == 3 ? 1 : -1) * (fScintillatorSize.x() + cutBoxTollerance);
		cutPositionBox[1] = (fLogical == 3 || fLogical == 4 ? 1 : -1) * (fScintillatorSize.y() + cutBoxTollerance);

		cutPositionAir[0] = (fLogical==1 || fLogical == 3 ? 1 : -1 ) * (fScintillatorSize.x() - fSkinWidth + cutBoxTollerance);
        cutPositionAir[1] = (fLogical==3 || fLogical == 4 ? 1 : -1 ) * (fScintillatorSize.y() - fSkinWidth + cutBoxTollerance);

		cutPositionCore[0] = (fLogical == 1 || fLogical == 3 ? 1 : -1) * (fBareScintillatorSize.x() + cutBoxTollerance);
		cutPositionCore[1] = (fLogical == 3 || fLogical == 4 ? 1 : -1) * (fBareScintillatorSize.y() + cutBoxTollerance);


		// Produce trapezoidal shapes on the inner end of central outer scintillators (4 in total)
		G4VSolid* scintillator_mother_cut      = MakeCut(scintillator_mother, cutBoxSize/2., cutBoxSize/2., 1.001 * ScintMotherThickness/2., g4rot45, cutPositionMom);

		// Produce trapezoidal shapes on the inner end of central outer scintillators (4 in total)
		G4VSolid* scintillator_box_cut      = MakeCut(scintillator_box, cutBoxSize/2., cutBoxSize/2., 1.001 * fScintillatorSize.z(), g4rot45, cutPositionBox);

		// Produce trapezoidal shapes on the inner end of central outer scintillators (4 in total)
		G4VSolid* scintillator_box_AirGap_cut = MakeCut(scintillator_box_AirGap, cutBoxSize/2., cutBoxSize/2., 1.001 * (fScintillatorSize.z()-fSkinWidth), g4rot45, cutPositionAir);

		// Produce trapezoidal shapes on the inner end of central outer scintillators (4 in total)
		G4VSolid* scintillator_box_core_cut = MakeCut(scintillator_box_core, cutBoxSize/2., cutBoxSize/2., 1.005*fBareScintillatorSize.z(), g4rot45, cutPositionCore);


		// Subtract the AirGap volume from the full scintillator volume --> the latter becomes a thin volume of wrapper material
	    G4VSolid * scintillator_box_cut_empty = new G4SubtractionSolid("scintillator_box_cut_empty",
	    							scintillator_box_cut,
	    							scintillator_box_AirGap_cut,
	                                0,
	                                G4ThreeVector(0,0,0.));

	    // Make fiber holes in the wrapper volume
	    G4VSolid* scintillator_box_cut_empty_holes = Make2Holes(scintillator_box_cut_empty, fLogical);



		////////////////////////
		// Logical Volumes


		// Logical volume of the bare scintillator
		fLogicalVolume = new G4LogicalVolume( scintillator_box_core_cut,
				fMaterial, // material
				"MUV1Scintillator", 0, 0, 0);


	    // Logical volume of the scintillator wrapper
		scintillator_box_log = new G4LogicalVolume(scintillator_box_cut_empty_holes,
					G4Material::GetMaterial("G4_Al"),
					"MUV1ScintillatorAlu", 0, 0,	0);

		//		G4VisAttributes* VisAttAlu = new G4VisAttributes(G4Colour(1.0, 0.0, 0.1, 0.0));
		//		VisAttAlu -> SetVisibility(true);	scintillator_box_log ->SetVisAttributes(VisAttAlu);


		// Logical volume for the scintillator mother
		scintillator_mother_log = new G4LogicalVolume( scintillator_mother_cut,
				G4Material::GetMaterial("G4_AIR"),
				"MUV1ScintillatorMother", 0, 0, 0);

		//		G4VisAttributes* VisAttMother = new G4VisAttributes(G4Colour(1.0, 0.0, 0.1, 1.0));
		//		VisAttMother -> SetVisibility(true);		scintillator_mother_log ->SetVisAttributes(VisAttMother);


	} else {

		// Subtract the AirGap volume from the full volume --> the latter becomes a thin volume of wrapper material
	    G4VSolid * scintillator_box_empty = new G4SubtractionSolid("scintillator_box_empty",
	    							scintillator_box,
	    							scintillator_box_AirGap,
	                                0,
	                                G4ThreeVector(0,0,0.));

		G4VSolid * scintillator_box_empty_holes = Make4Holes(scintillator_box_empty,  fboolOpp, fScintillatorPosition);


		////////////////////////
		// Logical Volumes

		//Scintillator
		fLogicalVolume = new G4LogicalVolume(scintillator_box_core, //
				fMaterial, // material
				"MUV1Scintillator",
				0, 0, 0);

		// Wrapper
		scintillator_box_log = new G4LogicalVolume(scintillator_box_empty_holes,
				G4Material::GetMaterial("G4_Al"),
				"MUV1ScintillatorAlu",
				0, 0,	0);

		//		G4VisAttributes* VisAttAlu = new G4VisAttributes(G4Colour(0.0, 0.0, 0.1, 0.0));
		//		VisAttAlu -> SetVisibility(true);	scintillator_box_log ->SetVisAttributes(VisAttAlu);

		//Mother
		scintillator_mother_log = new G4LogicalVolume( scintillator_mother,
				G4Material::GetMaterial("G4_AIR"),
				"MUV1ScintillatorMother",
				0, 0, 0);

		//		G4VisAttributes* VisAttMother = new G4VisAttributes(G4Colour(1.0, 0.0, 0.1, 0.0));
		//		VisAttMother -> SetVisibility(true);		scintillator_box_core_log ->SetVisAttributes(VisAttMother);

	}


	G4ThreeVector ScintPosition(0,0,0);
	if(fboolOpp)	{ //half size channels
		G4double posYscint = 0.5 * ScintMotherLength - fScintillatorSize.y() - fAirGapWidth/3.;
		if(fScintillatorPosition.y()>0) posYscint *= -1.;
		ScintPosition.setY( posYscint );
	}

	// Scintillator core with the scintillating material
	fPhysicalVolume = new G4PVPlacement(0, // no rotation
    		  ScintPosition,
    		  fLogicalVolume, // its logical volume
		      "MUV1ScintillatorPV", // its name
		      scintillator_mother_log, // its mother volume
		      false, // no boolean operations
		      fiCopy);

	// Scintillator wrapper (aluminium)
    G4VPhysicalVolume* PhysicalVolumeAlu = new G4PVPlacement(0, // no rotation
    		  ScintPosition,
		      scintillator_box_log, // its logical volume
		      "MUV1ScintillatorAluPV", // its name
		      scintillator_mother_log, // its mother volume
		      false, // no boolean operations
		      fiCopy);


	G4ThreeVector ScintMotherPosition(fScintillatorPosition.x(), fScintillatorPosition.y(), fScintillatorPosition.z());

	//for a half size scitnillator its center doesn't coincide with the center of its mother volume. Calculate a corresponding shift
	if(fboolOpp)	{ //half size channels
		G4double yShift = 0.5 * ScintMotherLength - fScintillatorSize.y() - fAirGapWidth/3.;
		G4double posY   = (fScintillatorPosition.y() > 0) ? fScintillatorPosition.y() + yShift  :  fScintillatorPosition.y() - yShift ;
		ScintMotherPosition.setY( posY );
	}

	// Scintillator mother volume
	new G4PVPlacement(fTransform,
			ScintMotherPosition, // replica position
			scintillator_mother_log, // its logical volume
			"MUV1ScintillatorMotherPV", // its name
			fMotherVolume, // its mother  volume
			false, // no boolean operations
			fiCopy); // copy number




	// Grooves with fibers
	G4ThreeVector groovePositionL = G4ThreeVector(-fGroovePositionInStandardScintillator, 0, fBareScintillatorSize.z() - 0.5 * fGrooveDepth);
	G4ThreeVector groovePositionR = G4ThreeVector( fGroovePositionInStandardScintillator, 0, fBareScintillatorSize.z() - 0.5 * fGrooveDepth);


	//07.11.2017
	//TODO: this parameter should be +1. But that implies proper G4 logics in assembling fibers into grooves and then both in scintillator core volume.
	//For the fast simulation, which is the only working option at the moment, the proper G4 logics (not implemented yet) is not needed,
	//since fibers don't have to extend out of grooves. Therefore, at the moment fiber volumes can be contained in
	//groove volumes entirely and the groove volumes themselves can be entirely contained inside the scintillator core volumes.
	G4int deltaLengthSign = -1;

	if(fLogical==2  || fLogical == 4)	{
		groovePositionL[1] = ((fLogical == 2) ? 1 : -1) * (0.5 * fCutDepthLong  - 0.26 * fGrooveWidth * deltaLengthSign);
		groovePositionR[1] = ((fLogical == 2) ? 1 : -1) * (0.5 * fCutDepthShort - 0.26 * fGrooveWidth * deltaLengthSign);
	}
	else if(fLogical==1  || fLogical == 3)	{
		groovePositionL[1] = ((fLogical == 1) ? 1 : -1) * (0.5 * fCutDepthShort - 0.26 * fGrooveWidth * deltaLengthSign);
		groovePositionR[1] = ((fLogical == 1) ? 1 : -1) * (0.5 * fCutDepthLong  - 0.26 * fGrooveWidth * deltaLengthSign);
	}

	G4double grooveLengthL = 2 * fBareScintillatorSize.y();
	G4double grooveLengthR = 2 * fBareScintillatorSize.y();

	if(fLogical==2  || fLogical == 4)	{
		grooveLengthL = 2 * fBareScintillatorSize.y() - fCutDepthLong  + 0.52 * fGrooveWidth * deltaLengthSign;
		grooveLengthR = 2 * fBareScintillatorSize.y() - fCutDepthShort + 0.52 * fGrooveWidth * deltaLengthSign;
	}
	else if(fLogical==1  || fLogical == 3)	{
		grooveLengthL = 2 * fBareScintillatorSize.y() - fCutDepthShort + 0.52 * fGrooveWidth * deltaLengthSign;
		grooveLengthR = 2 * fBareScintillatorSize.y() - fCutDepthLong  + 0.52 * fGrooveWidth * deltaLengthSign;
	}


	//Which material for the WLS fibers?
	G4Material* WLSMat =  G4Material::GetMaterial("DummyWLSMatCore");
	if(fMaterial->GetName()=="Scint"){ //this means: Full simulation
		WLSMat = G4Material::GetMaterial("WLSMatCore");
	}

	// Placing the grooves with the fibers inside
	MUV1Groove* grooveL = new MUV1Groove(WLSMat, fLogicalVolume, fGrooveWidth,
			fGrooveDepth, fFiberOuterRadius, grooveLengthL, groovePositionL, 0);

	MUV1Groove* grooveR = new MUV1Groove(WLSMat, fLogicalVolume, fGrooveWidth,
			fGrooveDepth, fFiberOuterRadius, grooveLengthR, groovePositionR, 0);



	G4SDManager* SDman = G4SDManager::GetSDMpointer();
	G4String MUV1SDname = "/MUV1";
	G4VSensitiveDetector * Muv1SD = SDman->FindSensitiveDetector(MUV1SDname);
	fLogicalVolume->SetSensitiveDetector(Muv1SD);


	//FULL SIMULATION
	// Wrapping of scintillator

	G4OpticalSurface* ScintWrap   = new G4OpticalSurface("ScintWrap");
	G4OpticalSurface* FiberFinish = new G4OpticalSurface("FiberFinish");

	//	new G4LogicalBorderSurface("ScintWrap", scintillator_box_AirGap_phys, PhysicalVolumeAlu, ScintWrap);
	new G4LogicalBorderSurface("ScintWrap", grooveR->GetGroovePhys(),	  PhysicalVolumeAlu, ScintWrap);
	new G4LogicalBorderSurface("ScintWrap", grooveL->GetGroovePhys(),	  PhysicalVolumeAlu, ScintWrap);

	//	new G4LogicalBorderSurface("FiberFinish",
	//			grooveL->GetFiber()->GetFiberCorePhysVolume(), scintillator_box_AirGap_phys,
	//			FiberFinish);

	new G4LogicalBorderSurface("FiberFinish",
			grooveL->GetFiber()->GetFiberCorePhysVolume(), PhysicalVolumeAlu,
			FiberFinish);

	//	new G4LogicalBorderSurface("FiberFinish",
	//			grooveR->GetFiber()->GetFiberCorePhysVolume(), scintillator_box_AirGap_phys,
	//			FiberFinish);

	new G4LogicalBorderSurface("FiberFinish",
			grooveR->GetFiber()->GetFiberCorePhysVolume(), PhysicalVolumeAlu,
			FiberFinish);

	//  set the correct model here
	ScintWrap->SetType(dielectric_metal);
	ScintWrap->SetFinish(polished);
	ScintWrap->SetModel(glisur);

	FiberFinish->SetType(dielectric_metal);
	FiberFinish->SetFinish(polished);
	FiberFinish->SetModel(glisur);
	// ***

	const G4int NUM = 2;

	G4double pp[NUM] = { 0.1 * eV, 5 * eV };
	G4double reflectivity1[NUM] = { 1, 1 };
	G4double reflectivity0[NUM] = { 0, 0 };
	G4double efficiency[NUM] = { 0.0, 0.0 };

	G4MaterialPropertiesTable* ScintWrapProperty = new G4MaterialPropertiesTable();
	G4MaterialPropertiesTable* FiberFinishProperty = new G4MaterialPropertiesTable();

	ScintWrapProperty->AddProperty("REFLECTIVITY", pp, reflectivity1, NUM);
	ScintWrapProperty->AddProperty("EFFICIENCY", pp, efficiency, NUM);
	ScintWrap->SetMaterialPropertiesTable(ScintWrapProperty);

	FiberFinishProperty->AddProperty("REFLECTIVITY", pp, reflectivity0, NUM);
	FiberFinishProperty->AddProperty("EFFICIENCY", pp, efficiency, NUM);
	FiberFinish->SetMaterialPropertiesTable(FiberFinishProperty);


}

void MUV1Scintillator::SetProperties() {
	// Set visualization properties
	fVisAtt = new G4VisAttributes(G4Colour(0.0, 0.0, 0.1, 1.0));
	fVisAtt -> SetVisibility(true);
	fLogicalVolume ->SetVisAttributes(fVisAtt);
}

// Function to cut the scintillator edges
G4VSolid* MUV1Scintillator::MakeCut(G4VSolid* box, G4double CutBoxHalfSizeX, G4double CutBoxHalfSizeY, G4double CutBoxHalfSizeZ, G4RotationMatrix* g4rot, G4ThreeVector CutPosition) {

	G4Box* cutBox = new G4Box("CutBox", CutBoxHalfSizeX, CutBoxHalfSizeY, CutBoxHalfSizeZ);

	G4VSolid* box_cut = new G4SubtractionSolid("box_cut", box, cutBox, g4rot, CutPosition);

	return box_cut;
}

// Function to cut two holes on one side. Needed for the inner scintillators arround beam pipe
G4VSolid* MUV1Scintillator::Make2Holes(G4VSolid* box, G4int Logical) {
	 /// \MemberDescr
	 /// This function cuts holes for fibers into the wrapping at the outer ends of the half size scintillators, which are only read out on one side.
	 /// \EndMemberDescr

	G4RotationMatrix* g4rot = new G4RotationMatrix(CLHEP::HepRotationX(90.*deg));
	*g4rot = g4rot->inverse();
	G4double HoleInnerRadius = 0 * mm;

	// Cutting tube
	G4Tubs* hole_tube = new G4Tubs("hole_tube", HoleInnerRadius, 1.001 * fFiberOuterRadius, 1.01 * 0.5 * fSkinWidth, 0. * deg, 360. * deg);

	G4VSolid * scintillator_box_hole = new G4SubtractionSolid(
			"scintillator_box", box, hole_tube, g4rot,
			G4ThreeVector( fGroovePositionInStandardScintillator,
					       ((Logical == 1 || Logical == 2) ? 1 : -1) * (fScintillatorSize.y() - 0.5 * fSkinWidth),
					       fScintillatorSize.z() - 0.5 * fGrooveDepth));

	G4VSolid * scintillator_box_hole2 = new G4SubtractionSolid(
			"scintillator_box", scintillator_box_hole, hole_tube, g4rot,
			G4ThreeVector(-fGroovePositionInStandardScintillator,
					       ((Logical== 1 || Logical == 2) ? 1 : -1) * (fScintillatorSize.y() - 0.5 * fSkinWidth),
					       fScintillatorSize.z() - 0.5 * fGrooveDepth));

	return scintillator_box_hole2;
}

// Function to cut four holes in the long scintillators
G4VSolid* MUV1Scintillator::Make4Holes(G4VSolid* box, G4bool boolOpp, G4ThreeVector position) {
	 /// \MemberDescr
	 /// This function cuts 2 or 4 holes for fibers into the wrapping of scintillators which are read out from only one or both sides
	 /// \EndMemberDescr

	G4RotationMatrix* g4rot = new G4RotationMatrix(CLHEP::HepRotationX(90.*deg));
	*g4rot = g4rot->inverse();

	G4double HoleInnerRadius = 0 * mm;

	// Cutting tube
	G4Tubs* hole_tube = new G4Tubs("hole_tube", HoleInnerRadius, 1.001 * fFiberOuterRadius, 1.01 * 0.5 * fSkinWidth, 0. * deg, 360. * deg);

	if (boolOpp) {

		G4VSolid * scintillator_box_hole = new G4SubtractionSolid(
				"scintillator_box", box, hole_tube, g4rot,
				G4ThreeVector( fGroovePositionInStandardScintillator,
						       (position[1] > 0 ? 1 : -1) * (fScintillatorSize.y() - 0.5 * fSkinWidth),
						       fScintillatorSize.z() - 0.5 * fGrooveDepth));

		G4VSolid * scintillator_box_hole2 = new G4SubtractionSolid(
				"scintillator_box", scintillator_box_hole, hole_tube, g4rot,
				G4ThreeVector(-fGroovePositionInStandardScintillator,
						       (position[1] > 0 ? 1 : -1) * (fScintillatorSize.y() - 0.5 * fSkinWidth),
						       fScintillatorSize.z() - 0.5 * fGrooveDepth));

		return scintillator_box_hole2;
	}

	G4VSolid * scintillator_box_hole = new G4SubtractionSolid(
			"scintillator_box", box, hole_tube, g4rot,
			G4ThreeVector( fGroovePositionInStandardScintillator,
					      -(fScintillatorSize.y() - 0.5 * fSkinWidth),
					       fScintillatorSize.z() - 0.5	* fGrooveDepth));

	G4VSolid * scintillator_box_hole2 = new G4SubtractionSolid(
			"scintillator_box", scintillator_box_hole, hole_tube, g4rot,
			G4ThreeVector(-fGroovePositionInStandardScintillator,
					      -(fScintillatorSize.y() - 0.5 * fSkinWidth),
					       fScintillatorSize.z() - 0.5	* fGrooveDepth));

	G4VSolid * scintillator_box_hole3 = new G4SubtractionSolid(
			"scintillator_box", scintillator_box_hole2, hole_tube, g4rot,
			G4ThreeVector( fGroovePositionInStandardScintillator,
					       (fScintillatorSize.y() - 0.5 * fSkinWidth),
					       fScintillatorSize.z() - 0.5 * fGrooveDepth));

	G4VSolid * scintillator_box_hole4 = new G4SubtractionSolid(
			"scintillator_box", scintillator_box_hole3, hole_tube, g4rot,
			G4ThreeVector(-fGroovePositionInStandardScintillator,
					(fScintillatorSize.y() - 0.5 * fSkinWidth),
					(fScintillatorSize.z() - 2 * fSkinWidth) - 0.5
							* fGrooveDepth));

	return scintillator_box_hole4;
}
