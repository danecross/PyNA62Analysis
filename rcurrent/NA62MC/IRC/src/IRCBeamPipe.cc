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
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// 2010-11-10 Spasimir Balev
//            -- Change geometry according to TDR
//
// --------------------------------------------------------------------
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "IRCGeometryParameters.hh"
#include "IRCMaterialParameters.hh"

#include "IRCBeamPipe.hh"
#include "IRCSegment.hh"

IRCBeamPipe::IRCBeamPipe(G4Material * Material, G4LogicalVolume * MotherVolume, 
			 G4Transform3D Transform3D,
			 G4double ZLength, G4double InnerRadius, G4double OuterRadius, G4double DisplacementX, G4int iCopy) : 
  NA62VComponent(Material,MotherVolume),
  fiCopy(iCopy),
  fTransform3D(Transform3D),
  fZLength(ZLength),
  fInnerRadius(InnerRadius),
  fOuterRadius(OuterRadius),
  fDisplacementX(DisplacementX)
{
  ReadGeometryParameters();
  // Mandatory here to Find or Build the needed materials
  IRCMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

IRCBeamPipe::~IRCBeamPipe(){}

void IRCBeamPipe::ReadGeometryParameters()
{
}

void IRCBeamPipe::CreateGeometry()
{
  G4double HalfZLength = 0.5*fZLength;
  
  G4Tubs* BeamTube = new G4Tubs("IRCBeamPipe",
				0.,
				fOuterRadius,
				HalfZLength,
				0,360*deg);   
  G4Tubs* BeamTubeHole = new G4Tubs("Hole",
				    0.,fInnerRadius,
				    HalfZLength*1.01,
				    0,360*deg);
  G4RotationMatrix* RotationHole = new G4RotationMatrix;
  G4ThreeVector Displacement = G4ThreeVector(fDisplacementX,0.,0.);
  fSolidVolume = new G4SubtractionSolid("IRCBeamPipe",BeamTube,BeamTubeHole,RotationHole,Displacement);

  fLogicalVolume= new G4LogicalVolume(fSolidVolume,                        // solid
				      fMaterial,                           // material
				      "IRCBeamPipe",                       // name
				      0,                                   // field manager 
				      0,                                   // sensitive detector
				      0);                                  // user limits
  fPhysicalVolume = new G4PVPlacement(fTransform3D,
				      fLogicalVolume,             // its logical volume
				      "IRCBeamPipe",                // its name
				      fMotherVolume,              // its mother  volume
				      false,                      // no boolean operations
				      fiCopy);                    // copy number
}

void IRCBeamPipe::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
