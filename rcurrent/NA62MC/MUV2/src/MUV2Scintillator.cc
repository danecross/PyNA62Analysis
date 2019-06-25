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
//Changes to MUV2 by ykohl in MArch 2010
//Now you can see the holes for the fibers and also the BoxCuts from ScintCut 
//
// ---------------------------------------------------------------------
// Mario Vormstein (mario.vormstein@cern.ch)  2011-02-01
//
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch) 2017-11-17
// --------------------------------------------------------------------


#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV2GeometryParameters.hh"
#include "MUV2MaterialParameters.hh"

#include "MUV2Scintillator.hh"
#include "MUV2SD.hh"
#include "G4SDManager.hh"


MUV2Scintillator::MUV2Scintillator(G4RotationMatrix* Transform,
		G4Material * Material,
		G4LogicalVolume * MotherVolume,
		G4ThreeVector ScintillatorSize,
		G4ThreeVector ScintillatorPosition,
		G4bool boolOpp,
		G4int iCopy,
		G4int Logical) :
	NA62VComponent(Material,MotherVolume),
	fiCopy     (iCopy),
	fScintillatorSize    (ScintillatorSize),
	fScintillatorPosition(ScintillatorPosition),
	fboolOpp(boolOpp),
	fLogical(Logical),
	fTransform(Transform)
{
	ReadGeometryParameters();

	// Mandatory here to Find or Build the needed materials
	MUV2MaterialParameters::GetInstance();

	CreateGeometry();
	SetProperties();
}

MUV2Scintillator::~MUV2Scintillator(){}

void MUV2Scintillator::ReadGeometryParameters()
{
	// Read all the geometrical parameters and copy them to private members
	MUV2GeometryParameters* GeoPars = MUV2GeometryParameters::GetInstance();

	fmOScintillatorCutHeight = GeoPars->GetmOScintillatorCutHeight();
	fmOScintillatorCutHeight2 = GeoPars->GetmOScintillatorCutHeight2();
	fScintillatorCutWidth = GeoPars->GetScintillatorCutWidth();
	fScintillatorCutWidth2 = GeoPars->GetScintillatorCutWidth2();
	fInnerRadius = 0.5*GeoPars->GetHoleDiameter();
	fSkinAluminumWidth = GeoPars->GetSkinAluminumWidth();
	fSkinTapeWidth = GeoPars->GetSkinTapeWidth();
	fAirGapWidth = GeoPars->GetAirGapWidth();
}

void MUV2Scintillator::CreateGeometry()
{

	// Build one or more boxes that will contain all the
	// detector sections, up to fill the responsibility region

	G4double ScintMotherExtraHalfSize = (fSkinAluminumWidth + 2*fAirGapWidth + fSkinTapeWidth) + fAirGapWidth ;

	G4VSolid* SolidVolumeMother= new G4Box("MUV2ScintillatorMotherStandSV",
			fScintillatorSize.x() + ScintMotherExtraHalfSize,
			fScintillatorSize.y() + ScintMotherExtraHalfSize,
			fScintillatorSize.z() + ScintMotherExtraHalfSize);

	G4double ScintTapeExtraHalfSize =  ScintMotherExtraHalfSize - fAirGapWidth ;

	G4VSolid* SolidVolumeTape= new G4Box("MUV2ScintillatorTapeStandSV",
			fScintillatorSize.x() + ScintTapeExtraHalfSize,
			fScintillatorSize.y() + ScintTapeExtraHalfSize,
			fScintillatorSize.z() + ScintTapeExtraHalfSize);

	G4double ScintAirGapOutExtraHalfSize =  ScintTapeExtraHalfSize - fSkinTapeWidth ;

	G4VSolid* SolidVolumeAirGapOut= new G4Box("MUV2ScintillatorAirGapOutStandSV",
			fScintillatorSize.x() + ScintAirGapOutExtraHalfSize,
			fScintillatorSize.y() + ScintAirGapOutExtraHalfSize,
			fScintillatorSize.z() + ScintAirGapOutExtraHalfSize);

	G4VSolid* SolidVolumeAirGapOut2= new G4Box("MUV2ScintillatorAirGapOut2StandSV",
			fScintillatorSize.x() + ScintAirGapOutExtraHalfSize - 10 * mm,
			fScintillatorSize.y() + ScintAirGapOutExtraHalfSize - 10 * mm,
			fScintillatorSize.z() + ScintMotherExtraHalfSize);

	G4double ScintAluExtraHalfSize =  ScintAirGapOutExtraHalfSize - fAirGapWidth ;

	G4VSolid* SolidVolumeAlu= new G4Box("MUV2ScintillatorAluStandSV",
			fScintillatorSize.x() + ScintAluExtraHalfSize,
			fScintillatorSize.y() + ScintAluExtraHalfSize,
			fScintillatorSize.z() + ScintAluExtraHalfSize);

	G4double ScintAirGapInExtraHalfSize =  ScintAluExtraHalfSize - fSkinAluminumWidth ;

	G4VSolid* SolidVolumeAirGapIn = new G4Box("MUV2ScintillatorAirGapInStandSV",
			fScintillatorSize.x() + ScintAirGapInExtraHalfSize,
			fScintillatorSize.y() + ScintAirGapInExtraHalfSize,
			fScintillatorSize.z() + ScintAirGapInExtraHalfSize);

	// Define Scintillator volume (a box)
	G4VSolid* SolidVolumeScint = new G4Box("MUV2ScintillatorStandSV",
			fScintillatorSize.x(),
			fScintillatorSize.y(),
			fScintillatorSize.z());


	fSolidVolume = SolidVolumeMother;

	//central 4 strips per layer, which have cut shapes to fit the beam pipe hole
	if(fboolOpp){

		//use fAirGapWidth variable as a tolerance parameter for cut boxes in order
		//to make sure that the corners are completely cut away and fix the round-off
		//overlaps with the mother scintillator layer volume.
		fCutBox = new G4Box("MUV2ScintillatorCutBox",
				0.5*fScintillatorCutWidth    + fAirGapWidth,
				0.5*fmOScintillatorCutHeight + fAirGapWidth,
				fScintillatorSize.z()        + ScintMotherExtraHalfSize + fAirGapWidth);

		fCutBox2 = new G4Box("MUV2ScintillatorCutBox2",
				0.5*fScintillatorCutWidth2    + fAirGapWidth,
				0.5*fmOScintillatorCutHeight2 + fAirGapWidth,
				fScintillatorSize.z()         + ScintMotherExtraHalfSize + fAirGapWidth);


		fCutBoxPos  = G4ThreeVector(0.,0.,0.);
		fCutBoxPos2 = G4ThreeVector(0.,0.,0.);
		G4double CutBoxCenterX, CutBox2CenterX;
		G4double CutBoxCenterY, CutBox2CenterY;
		G4int BasePosSignX=0,  ShiftPosSignX=0;
		G4int BasePosSignY=0,  ShiftPosSignY=0;
		G4int BasePos2SignX=0, ShiftPos2SignX=0;
		G4int BasePos2SignY=0, ShiftPos2SignY=0;


		if(fLogical == 1){ //(-X, +Y)
			BasePosSignX  = -1;	ShiftPosSignX  = 1;
			BasePosSignY  = -1;	ShiftPosSignY  = -1;
			BasePos2SignX = -1;	ShiftPos2SignX = 1;
			BasePos2SignY = -1;	ShiftPos2SignY = -1;
		}
		else if(fLogical == 2)	{//(+X, +Y)
			BasePosSignX  = 1;	ShiftPosSignX  = -1;
			BasePosSignY  = -1;	ShiftPosSignY  = -1;
			BasePos2SignX = 1;	ShiftPos2SignX = -1;
			BasePos2SignY = -1;	ShiftPos2SignY = -1;
		}
		else if(fLogical == 3)	{//(-X, -Y)
			BasePosSignX  = -1;	ShiftPosSignX  = 1;
			BasePosSignY  = 1;	ShiftPosSignY  = 1;
			BasePos2SignX = -1;	ShiftPos2SignX = 1;
			BasePos2SignY = 1;	ShiftPos2SignY = 1;
		}
		else if(fLogical == 4)	{//(+X, -Y)
			BasePosSignX  = 1;    ShiftPosSignX  = -1;
			BasePosSignY  = 1;	ShiftPosSignY  = 1;
			BasePos2SignX = 1;	ShiftPos2SignX = -1;
			BasePos2SignY = 1;	ShiftPos2SignY = 1;
		}


		// **************   cut mother SV   **********************

		CutBoxCenterX = BasePosSignX * (0.5*fScintillatorCutWidth - fScintillatorSize.x())    + ShiftPosSignX * (ScintMotherExtraHalfSize + fAirGapWidth);
		CutBoxCenterY = BasePosSignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight) + ShiftPosSignY * (ScintMotherExtraHalfSize + fAirGapWidth);
		fCutBoxPos.setX(CutBoxCenterX);	  fCutBoxPos.setY(CutBoxCenterY);

		CutBox2CenterX = BasePos2SignX * (fScintillatorCutWidth - fScintillatorSize.x() + 0.5*fScintillatorCutWidth2) + ShiftPos2SignX * (ScintMotherExtraHalfSize + fAirGapWidth);
		CutBox2CenterY = BasePos2SignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight2)                      + ShiftPos2SignY * (ScintMotherExtraHalfSize + fAirGapWidth);
		fCutBoxPos2.setX(CutBox2CenterX);	  fCutBoxPos2.setY(CutBox2CenterY);

		G4VSolid*  SolidVolumeMotherCutOnce  = new G4SubtractionSolid("MUV2ScintillatorMotherCutOnceSV" ,SolidVolumeMother,        fCutBox,  0, fCutBoxPos);
		G4VSolid*  SolidVolumeMotherCutTwice = new G4SubtractionSolid("MUV2ScintillatorMotherCutTwiceSV",SolidVolumeMotherCutOnce, fCutBox2, 0, fCutBoxPos2);

		//mother is the volume, which is positioned inside the scintillator layer volume. re-take the pointer to its final shape.
		fSolidVolume = SolidVolumeMotherCutTwice;


		// ***************   cut tape SV   **********************

		CutBoxCenterX = BasePosSignX * (0.5*fScintillatorCutWidth - fScintillatorSize.x())     + ShiftPosSignX * (ScintTapeExtraHalfSize + fAirGapWidth);
		CutBoxCenterY = BasePosSignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight)  + ShiftPosSignY * (ScintTapeExtraHalfSize + fAirGapWidth);
		fCutBoxPos.setX(CutBoxCenterX);	  fCutBoxPos.setY(CutBoxCenterY);

		CutBox2CenterX = BasePos2SignX * (fScintillatorCutWidth - fScintillatorSize.x() + 0.5*fScintillatorCutWidth2) + ShiftPos2SignX * (ScintTapeExtraHalfSize + fAirGapWidth);
		CutBox2CenterY = BasePos2SignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight2)                      + ShiftPos2SignY * (ScintTapeExtraHalfSize + fAirGapWidth);
		fCutBoxPos2.setX(CutBox2CenterX);	  fCutBoxPos2.setY(CutBox2CenterY);

		G4VSolid*  SolidVolumeTapeCutOnce  = new G4SubtractionSolid("MUV2ScintillatorTapeCutOnceSV" ,SolidVolumeTape,        fCutBox,  0, fCutBoxPos);
		G4VSolid*  SolidVolumeTapeCutTwice = new G4SubtractionSolid("MUV2ScintillatorTapeCutTwiceSV",SolidVolumeTapeCutOnce, fCutBox2, 0, fCutBoxPos2);

		SolidVolumeTape = SolidVolumeTapeCutTwice;


		// ***************   cut air gap outer SV   **********************

		CutBoxCenterX = BasePosSignX * (0.5*fScintillatorCutWidth - fScintillatorSize.x())     + ShiftPosSignX * (ScintAirGapOutExtraHalfSize + fAirGapWidth);
		CutBoxCenterY = BasePosSignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight)  + ShiftPosSignY * (ScintAirGapOutExtraHalfSize + fAirGapWidth);
		fCutBoxPos.setX(CutBoxCenterX);	  fCutBoxPos.setY(CutBoxCenterY);

		CutBox2CenterX = BasePos2SignX * (fScintillatorCutWidth - fScintillatorSize.x() + 0.5*fScintillatorCutWidth2) + ShiftPos2SignX * (ScintAirGapOutExtraHalfSize + fAirGapWidth);
		CutBox2CenterY = BasePos2SignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight2)                      + ShiftPos2SignY * (ScintAirGapOutExtraHalfSize + fAirGapWidth);
		fCutBoxPos2.setX(CutBox2CenterX);	  fCutBoxPos2.setY(CutBox2CenterY);

		G4VSolid*  SolidVolumeAirGapOutCutOnce  = new G4SubtractionSolid("MUV2ScintillatorAirGapOutCutOnceSV" ,SolidVolumeAirGapOut,        fCutBox,  0, fCutBoxPos);
		G4VSolid*  SolidVolumeAirGapOutCutTwice = new G4SubtractionSolid("MUV2ScintillatorAirGapOutCutTwiceSV",SolidVolumeAirGapOutCutOnce, fCutBox2, 0, fCutBoxPos2);

		SolidVolumeAirGapOut = SolidVolumeAirGapOutCutTwice;


		// ***************   cut air gap II outer SV   **********************

		CutBoxCenterX = BasePosSignX * (0.5*fScintillatorCutWidth - fScintillatorSize.x())     + ShiftPosSignX * (ScintAirGapOutExtraHalfSize-10*mm + fAirGapWidth);
		CutBoxCenterY = BasePosSignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight)  + ShiftPosSignY * (ScintAirGapOutExtraHalfSize-10*mm + fAirGapWidth);
		fCutBoxPos.setX(CutBoxCenterX);	  fCutBoxPos.setY(CutBoxCenterY);

		CutBox2CenterX = BasePos2SignX * (fScintillatorCutWidth - fScintillatorSize.x() + 0.5*fScintillatorCutWidth2) + ShiftPos2SignX * (ScintAirGapOutExtraHalfSize-10*mm + fAirGapWidth);
		CutBox2CenterY = BasePos2SignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight2)                      + ShiftPos2SignY * (ScintAirGapOutExtraHalfSize-10*mm + fAirGapWidth);
		fCutBoxPos2.setX(CutBox2CenterX);	  fCutBoxPos2.setY(CutBox2CenterY);

		G4VSolid*  SolidVolumeAirGapOut2CutOnce  = new G4SubtractionSolid("MUV2ScintillatorAirGapOut2CutOnceSV" ,SolidVolumeAirGapOut2,        fCutBox,  0, fCutBoxPos);
		G4VSolid*  SolidVolumeAirGapOut2CutTwice = new G4SubtractionSolid("MUV2ScintillatorAirGapOut2CutTwiceSV",SolidVolumeAirGapOut2CutOnce, fCutBox2, 0, fCutBoxPos2);

		SolidVolumeAirGapOut2 = SolidVolumeAirGapOut2CutTwice;


		// ***************  cut aluminum SV    **********************

		CutBoxCenterX = BasePosSignX * (0.5*fScintillatorCutWidth - fScintillatorSize.x())     + ShiftPosSignX * (ScintAluExtraHalfSize + fAirGapWidth);
		CutBoxCenterY = BasePosSignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight)  + ShiftPosSignY * (ScintAluExtraHalfSize + fAirGapWidth);
		fCutBoxPos.setX(CutBoxCenterX);	  fCutBoxPos.setY(CutBoxCenterY);

		CutBox2CenterX = BasePos2SignX * (fScintillatorCutWidth - fScintillatorSize.x() + 0.5*fScintillatorCutWidth2) + ShiftPos2SignX * (ScintAluExtraHalfSize + fAirGapWidth);
		CutBox2CenterY = BasePos2SignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight2)                      + ShiftPos2SignY * (ScintAluExtraHalfSize + fAirGapWidth);
		fCutBoxPos2.setX(CutBox2CenterX);	  fCutBoxPos2.setY(CutBox2CenterY);

		G4VSolid*  SolidVolumeAluCutOnce  = new G4SubtractionSolid("MUV2ScintillatorAluCutOnceSV" ,SolidVolumeAlu,        fCutBox,  0, fCutBoxPos);
		G4VSolid*  SolidVolumeAluCutTwice = new G4SubtractionSolid("MUV2ScintillatorAluCutTwiceSV",SolidVolumeAluCutOnce, fCutBox2, 0, fCutBoxPos2);

		SolidVolumeAlu = SolidVolumeAluCutTwice ;


		// ***************  cut air gap inner SV     **********************

		CutBoxCenterX = BasePosSignX * (0.5*fScintillatorCutWidth - fScintillatorSize.x())     + ShiftPosSignX * (ScintAirGapInExtraHalfSize + fAirGapWidth);
		CutBoxCenterY = BasePosSignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight)  + ShiftPosSignY * (ScintAirGapInExtraHalfSize + fAirGapWidth);
		fCutBoxPos.setX(CutBoxCenterX);	  fCutBoxPos.setY(CutBoxCenterY);

		CutBox2CenterX = BasePos2SignX * (fScintillatorCutWidth - fScintillatorSize.x() + 0.5*fScintillatorCutWidth2) + ShiftPos2SignX * (ScintAirGapInExtraHalfSize + fAirGapWidth);
		CutBox2CenterY = BasePos2SignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight2)                      + ShiftPos2SignY * (ScintAirGapInExtraHalfSize + fAirGapWidth);
		fCutBoxPos2.setX(CutBox2CenterX);	  fCutBoxPos2.setY(CutBox2CenterY);

		G4VSolid*  SolidVolumeAirGapInCutOnce  = new G4SubtractionSolid("MUV2ScintillatorAirGapInCutOnceSV" ,SolidVolumeAirGapIn,        fCutBox,  0, fCutBoxPos);
		G4VSolid*  SolidVolumeAirGapInCutTwice = new G4SubtractionSolid("MUV2ScintillatorAirGapInCutTwiceSV",SolidVolumeAirGapInCutOnce, fCutBox2, 0, fCutBoxPos2);

		SolidVolumeAirGapIn = SolidVolumeAirGapInCutTwice;


		// ***************  cut scintillator SV   **********************

		CutBoxCenterX = BasePosSignX * (0.5*fScintillatorCutWidth - fScintillatorSize.x())     + ShiftPosSignX * fAirGapWidth;
		CutBoxCenterY = BasePosSignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight)  + ShiftPosSignY * fAirGapWidth;
		fCutBoxPos.setX(CutBoxCenterX);	  fCutBoxPos.setY(CutBoxCenterY);

		CutBox2CenterX = BasePos2SignX * (fScintillatorCutWidth - fScintillatorSize.x() + 0.5*fScintillatorCutWidth2) + ShiftPos2SignX * fAirGapWidth;
		CutBox2CenterY = BasePos2SignY * (fScintillatorSize.y() - 0.5*fmOScintillatorCutHeight2)                      + ShiftPos2SignY * fAirGapWidth;
		fCutBoxPos2.setX(CutBox2CenterX);	  fCutBoxPos2.setY(CutBox2CenterY);

		G4VSolid*  SolidVolumeScintCutOnce  = new G4SubtractionSolid("MUV2ScintillatorCutOnceSV",SolidVolumeScint,        fCutBox,  0, fCutBoxPos);
		G4VSolid*  SolidVolumeScintCutTwice = new G4SubtractionSolid("MUV2ScintillatorMiddleSV", SolidVolumeScintCutOnce, fCutBox2, 0, fCutBoxPos2);

		SolidVolumeScint = SolidVolumeScintCutTwice;

	}


	// Make solid tape and aluminum volumes empty volumes with thin walls. In case of the tape volume apply one more cut to make it a rectangular C-profile like shape

	G4VSolid* SolidVolumeTapeEmpty    = new G4SubtractionSolid("MUV2ScintillatorTapeEmptySV",    SolidVolumeTape,      SolidVolumeAirGapOut,  0, G4ThreeVector(0.,0.,0.));
	G4VSolid* SolidVolumeTapeCProfile = new G4SubtractionSolid("MUV2ScintillatorTapeCProfileSV", SolidVolumeTapeEmpty, SolidVolumeAirGapOut2, 0, G4ThreeVector(0.,0.,0.));
	G4VSolid* SolidVolumeAluEmpty     = new G4SubtractionSolid("MUV2ScintillatorAluEmptySV",     SolidVolumeAlu,       SolidVolumeAirGapIn,   0, G4ThreeVector(0.,0.,0.));


	//Cutting out light-trough windows in the wrapper materials (tape and aluminum foil) at the outer ends of stintillators

	fCutBox = new G4Box("MUV2ScintillatorEndCutBox",
			fScintillatorSize.x() - 20*mm,  //20 mm of wrapper material on each side (along the x-axis) of the outer ends of strips is not cut.
			//So, a centered window for light-trough at the outer end of strips is only 119(108)-2*20=79(68) mm wide
			(ScintTapeExtraHalfSize + fAirGapWidth),
			fScintillatorSize.z() + (ScintTapeExtraHalfSize + fAirGapWidth) );

	G4double EndCutBoxCenterY = fScintillatorSize.y() + (ScintTapeExtraHalfSize + fAirGapWidth);
	if(fScintillatorPosition.y()<0) EndCutBoxCenterY  *= -1;

	G4VSolid* SolidVolumeTapeEmptyOpen = new G4SubtractionSolid("MUV2ScintillatorTapeSV", SolidVolumeTapeCProfile, fCutBox, 0, G4ThreeVector(0.,EndCutBoxCenterY,0.));
	G4VSolid* SolidVolumeAluEmptyOpen  = new G4SubtractionSolid("MUV2ScintillatorAluSV",  SolidVolumeAluEmpty,     fCutBox, 0, G4ThreeVector(0.,EndCutBoxCenterY,0.));



	// ***  LOGICAL VOLUMES  ***

	G4LogicalVolume* LogicalVolumeMother = new G4LogicalVolume( fSolidVolume,       // solid
			G4Material::GetMaterial("G4_AIR"),
			((!fboolOpp) ? "MUV2ScintillatorMother" : "MUV2ScintillatorMiddleMother"), // name
			0,                  		// field manager
			0,                  // sensitive detector
			0);                 // user limits

	G4VisAttributes* VisAttMother= new G4VisAttributes(G4Colour(0.,0.,1.,1.0));
	VisAttMother -> SetVisibility(true);
	LogicalVolumeMother ->SetVisAttributes(VisAttMother);


	G4LogicalVolume* LogicalVolumeTape = new G4LogicalVolume(SolidVolumeTapeEmptyOpen,
			G4Material::GetMaterial("G4_POLYPROPYLENE"), //good guess of the material for the transparent tape used for wrapping the scitnillators
			((!fboolOpp) ? "MUV2ScintillatorTape" : "MUV2ScintillatorMiddleTape"), // name
			0,                  	   // field manager
			0,                  // sensitive detector
			0);                 // user limits

	G4VisAttributes* VisAttTape= new G4VisAttributes(G4Colour(1.,0.,0.,1.));
	VisAttTape -> SetVisibility(true);
	LogicalVolumeTape ->SetVisAttributes(VisAttTape);


	G4LogicalVolume* LogicalVolumeAlu = new G4LogicalVolume(SolidVolumeAluEmptyOpen,
			G4Material::GetMaterial("G4_Al"),
			((!fboolOpp) ? "MUV2ScintillatorAlu": "MUV2ScintillatorMiddleAlu"), // name
			0,                  	   // field manager
			0,                  // sensitive detector
			0);                 // user limits

	G4VisAttributes* VisAttAlu= new G4VisAttributes(G4Colour(0.,1.,0.,1.));
	VisAttAlu -> SetVisibility(true);
	LogicalVolumeAlu ->SetVisAttributes(VisAttAlu);


	fLogicalVolume = new G4LogicalVolume(SolidVolumeScint,
			fMaterial,
			((!fboolOpp) ?"MUV2Scintillator" : "MUV2ScintillatorMiddle"), // name
			0,                  	   // field manager
			0,                  // sensitive detector
			0);                 // user limits



	// ***  PHYSICAL VOLUMES  ***

	G4ThreeVector Position(0,0,0);

	//place the components inside the scintillator mother volume
	new G4PVPlacement(0,                  // no rotation
			Position,       				// replica position
			LogicalVolumeTape,               // logical volume
			((!fboolOpp) ? "MUV2ScintillatorTapePV" : "MUV2ScintillatorMiddleTapePV"),           // its name
			LogicalVolumeMother,                // its mother  volume
			false,                        // no boolean operations
			fiCopy);                      // copy number

	new G4PVPlacement(0,                  // no rotation
			Position,       				// replica position
			LogicalVolumeAlu,               // logical volume
			((!fboolOpp) ? "MUV2ScintillatorAluPV" : "MUV2ScintillatorMiddleAluPV"),           // its name
			LogicalVolumeMother,                // its mother  volume
			false,                        // no boolean operations
			fiCopy);                      // copy number

	new G4PVPlacement(0,                  // no rotation
			Position,       				// replica position
			fLogicalVolume,               // logical volume
			((!fboolOpp) ? "MUV2ScintillatorPV" : "MUV2ScintillatorMiddlePV"),           // its name
			LogicalVolumeMother,                // its mother  volume
			false,                        // no boolean operations
			fiCopy);                      // copy number


	//place the scintillator mother volume in the layer volume (external pointer)
	fPhysicalVolume = new G4PVPlacement(0,                  // no rotation
			fScintillatorPosition,       				// replica position
			LogicalVolumeMother,               // logical volume
			((!fboolOpp) ? "MUV2ScintillatorMotherPV" : "MUV2ScintillatorMiddleMotherPV" ),           // its name
			fMotherVolume,                // its mother  volume
			false,                        // no boolean operations
			fiCopy);                      // copy number



	// *** SENSITIVE DETECTOR ***

	G4SDManager* SDman = G4SDManager::GetSDMpointer();
	G4String MUV2SDname = "/MUV2";
	G4VSensitiveDetector * Muv2SD = SDman->FindSensitiveDetector(MUV2SDname);
	fLogicalVolume->SetSensitiveDetector(Muv2SD);
}

void MUV2Scintillator::SetProperties()
{
	// Set visualization properties
	fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
	fVisAtt -> SetVisibility(true);
	fLogicalVolume ->SetVisAttributes(fVisAtt);
}
