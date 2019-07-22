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
//Changes to MUV2 by ykohl in March 2010
//
//Modified by Gia Khoriauli (gia.khoriauli@cern.ch) 2017-11-17
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

#include "MUV2ScintillatorLayer.hh"
#include "MUV2Scintillator.hh"



MUV2ScintillatorLayer::MUV2ScintillatorLayer(G4Material * Material, 
                                 G4LogicalVolume * MotherVolume,
                                 G4ThreeVector ScintLayerPosition,
                                 G4int iCopy, G4int Offset) : NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();

  fiCopy = iCopy;
  fOffset = Offset;
  
  fScintLayerPosition = ScintLayerPosition;
  
  // Mandatory here to Find or Build the needed materials
  MUV2MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

MUV2ScintillatorLayer::~MUV2ScintillatorLayer(){}

void MUV2ScintillatorLayer::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  MUV2GeometryParameters* GeoPars = MUV2GeometryParameters::GetInstance();

  // thickness of each scintillator (zsci)
  fScintillatorThickness = GeoPars->GetScintillatorThickness();
  //Length of scintillators
  fScintLengthStandard   = GeoPars->GetScintLengthStandard();
  fScintLengthMiddle     = GeoPars->GetScintLengthMiddle();
  fScintWidthStandard    = GeoPars->GetScintWidthStandard();
  fScintWidthMiddle      = GeoPars->GetScintWidthMiddle();

  fSkinAluminumWidth     = GeoPars->GetSkinAluminumWidth();
  fSkinTapeWidth         = GeoPars->GetSkinTapeWidth();
  fAirGapWidth           = GeoPars->GetAirGapWidth();

  fInnerRadius           = 0.5*GeoPars->GetHoleDiameter(); // MUV2 hole inner radius
}

void MUV2ScintillatorLayer::CreateGeometry()
{
  // Build one or more boxes that will contain all the 
  // detector sections, up to fill the responsibility region

  //Debugging
  //  G4double prevXpos = 0.;

  //total length of two opposing normal length strips. 4*fAirGapWidth is a gap between the two strips.
  //the additional 2*2*fAirGapWidth is for the outer ends of the strips.
  G4double ScintVolumeTotalLength        = 2*( 2*fSkinTapeWidth + 4*fAirGapWidth + 2*fSkinAluminumWidth + fScintLengthStandard )     + 8 * fAirGapWidth;
  G4double ScintVolumeTotalWidthStandard =     2*fSkinTapeWidth + 4*fAirGapWidth + 2*fSkinAluminumWidth + fScintWidthStandard        + 4 * fAirGapWidth;
  G4double ScintVolumeTotalWidthMiddle   =     2*fSkinTapeWidth + 4*fAirGapWidth + 2*fSkinAluminumWidth + fScintWidthMiddle          + 4 * fAirGapWidth;
  G4double ScintVolumeTotalThickness     =     2*fSkinTapeWidth + 4*fAirGapWidth + 2*fSkinAluminumWidth + fScintillatorThickness	 + 4 * fAirGapWidth;
  
  // Define Scintillator layer volume (a box)
  //a shape of layer is not a square but a rectangle extruded on the transverse direction (wrt the longest side of strips) due to the wrapping material and air gaps.
  //0.5 extra thickness is added to the layer length just as a tolerance to avoid round-off overlaps.
  fSolidVolume= new G4Box("MUV2ScintillatorLayer",
                          0.5*ScintVolumeTotalLength + (22.5+0.5) * (fSkinAluminumWidth+fSkinTapeWidth + 4*fAirGapWidth + 4*fAirGapWidth),
                          0.5*ScintVolumeTotalLength,
                          0.5*ScintVolumeTotalThickness);

  // Define Beam Pipe Hole volume (a cylinder); enlarge by 1.01 to see a clean hole in Vis
  G4Tubs * BeamPipeHole = new G4Tubs("MUV2BeamPipeHole",
                                     0.,
                                     fInnerRadius-0.02*fAirGapWidth, //keep this synchronized (means larger) with the outer radius of the MUV2IronTube
                                     1.01*0.5*ScintVolumeTotalThickness,
                                     0.*deg,
                                     360.*deg);
  
  // Subtract the Pipe Hole from the Scint layer volume
  G4VSolid * ScintLayerWithHole = new G4SubtractionSolid("MUV2ScintillatorLayerWithHole",
							 fSolidVolume,
							 BeamPipeHole,
							 0,
							 G4ThreeVector(0.,0.,0.));
  
  // Scint layer logical volume
  fLogicalVolume= new G4LogicalVolume(ScintLayerWithHole,           // solid
                                      fMaterial,                   // material
                                      "MUV2ScintillatorLayer",              // name
                                      0,                           // field manager 
                                      0,                           // sensitive detector
                                      0);                          // user limits
  
  // Scint layer physical volume
  fPhysicalVolume = new G4PVPlacement(0,                           // no rotation
                                      fScintLayerPosition,          // position
                                      fLogicalVolume,              // its logical volume
                                      "MUV2ScintillatorLayer",              // its name
                                      fMotherVolume,               // its mother  volume
                                      false,                       // no boolean operations
                                      fiCopy);                     // copy number
  
  // Loop over all scintillators in this layer
  G4Material* ScintMat = G4Material::GetMaterial("G4_PLASTIC_SC_VINYLTOLUENE"); // scint material
  
  //Placing scintillators in a layer
  for (int scintillatorNumber = 1; scintillatorNumber <= 44; scintillatorNumber++){

    G4double ScintDistanceFromCenter = fabs(0.5*(scintillatorNumber - (44/2 + 0.5) ));
    
    // Scintillators (type 1) on left side (x-Position negative) on right side (x-Position positive)
    G4double ScintPosX = (scintillatorNumber < 22.5 ? -1. : 1.) * (ScintVolumeTotalWidthMiddle +  ((G4double)((G4int)ScintDistanceFromCenter) - 0.5)*ScintVolumeTotalWidthStandard );
    
    G4double ScintPosY = (scintillatorNumber % 2 == 0 ? 1 : -1) * ( 0.25*(ScintVolumeTotalLength-8*fAirGapWidth) + 2*fAirGapWidth );


    // Scintillators of type 1
    G4ThreeVector ScintillatorSize = G4ThreeVector( 0.5*fScintWidthStandard,
			  	                                    0.5*fScintLengthStandard,
			  	                                    0.5*fScintillatorThickness);
    G4bool boolOpp = false;
    G4double Logical = -1;
  

    // The four inner scintillators
    if(ScintDistanceFromCenter < 1){ 

      ScintPosX = (scintillatorNumber < 22.5 ? -1. : 1) * 0.5 * ScintVolumeTotalWidthMiddle;
      ScintPosY = (scintillatorNumber % 2 == 0 ? 1 : -1) * ( fabs(ScintPosY) + 0.5*(fScintLengthStandard-fScintLengthMiddle) ) ;

      ScintillatorSize = G4ThreeVector(
				       0.5*fScintWidthMiddle,
				       0.5*fScintLengthMiddle,
				       0.5*fScintillatorThickness);

      boolOpp = true;        // cut for beam hole
      
      if(scintillatorNumber < 22.5){ Logical = (scintillatorNumber % 2 == 0 ? 1 : 3) ;}
      else{ Logical = (scintillatorNumber % 2 == 0 ? 2 : 4);}  
      
    }

    //Debugging ----->
    //    G4cout 	<< "scintillatorNumber " <<scintillatorNumber <<"   ScintDistanceFromCenter "<< ScintDistanceFromCenter << G4endl;
    //    G4cout	<<"   ScintPosX " << ScintPosX << "   ScintPosY "<< ScintPosY	<< "   Delta X position " << ScintPosX - prevXpos	<<"   Logical "<<Logical<< G4endl;
    //    prevXpos = ScintPosX ;
    //    if(ScintDistanceFromCenter < 1){
    //    	G4cout << "  Scintillator half length " << 0.25*(ScintVolumeTotalLength-8*fAirGapWidth) - 0.5*(fScintLengthStandard-fScintLengthMiddle) << G4endl;
    //    	G4cout << "  Scintillator half width  " << ScintVolumeTotalWidthMiddle/2. << G4endl;
    //    }    else	{
    //    	G4cout << "  Scintillator half length " << 0.25*(ScintVolumeTotalLength-8*fAirGapWidth) << G4endl;
    //    	G4cout << "  Scintillator half width  " << ScintVolumeTotalWidthStandard/2. << G4endl;
    //    }
    //<----- Debugging
    
    G4ThreeVector ScintillatorPosition = G4ThreeVector(ScintPosX, ScintPosY, 0);
    new MUV2Scintillator(0,
                         ScintMat,
                         fLogicalVolume,
                         ScintillatorSize,
                         ScintillatorPosition,
                         boolOpp,
                         scintillatorNumber+100*fiCopy+fOffset,
                         Logical);
  }
  
}


void MUV2ScintillatorLayer::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,0.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
