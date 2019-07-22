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

#include "CHANTIStation.hh"
#include "CHANTIRing.hh"

#include "CHANTISD.hh"
#include "G4SubtractionSolid.hh"
#include "G4UnionSolid.hh"
//
#include "CHANTISDSiPM.hh"

CHANTIStation::CHANTIStation(G4Material * Material, G4LogicalVolume * MotherVolume,int index) : 
NA62VComponent(Material,MotherVolume)
{
  fLogicalVolume = MotherVolume;
  fIndex = index;
  ReadGeometryParameters();
  // Mandatory here to Find or Build the needed materials
  CHANTIMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

CHANTIStation::~CHANTIStation(){}

void CHANTIStation::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  CHANTIGeometryParameters* GeoPars = CHANTIGeometryParameters::GetInstance();
  

  fZPosRings = GeoPars->GetZPos_Ring(fIndex);
  fRingThickness = GeoPars->GetCHANTIRingThickness();
  fXInnerHalfLength = GeoPars->GetCHANTIXInnerHoleLength()/2;
  fYInnerHalfLength = GeoPars->GetCHANTIYInnerHoleLength()/2;
  fSquareLength = GeoPars->GetCHANTISquareLength()/2;
}

void CHANTIStation::CreateGeometry()
{
  G4RotationMatrix* RotationX = new G4RotationMatrix();
  RotationX->rotateX(180.*deg);
  G4RotationMatrix* RotationZ = new G4RotationMatrix();
  RotationZ->rotateZ(-90.*deg);
  //vertical bars
  new CHANTIRing( fMaterial, 
		  fLogicalVolume, 
		  fZPosRings + fRingThickness/2., 
		  RotationX, 
		  fXInnerHalfLength, 
		  fYInnerHalfLength,  
		  fIndex*2 ); 
  //horizontal bars
  new CHANTIRing(fMaterial,
		  fLogicalVolume, 
		  fZPosRings + 3*fRingThickness/2., 
		  RotationZ, 
		  fYInnerHalfLength, 
		  fXInnerHalfLength, 
		  fIndex*2+1); 

}

void CHANTIStation::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
