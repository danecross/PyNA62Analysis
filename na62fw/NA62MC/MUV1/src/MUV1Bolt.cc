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
//---------------------------------------------------------------------
//
//
// Created by Mario Vormstein (mario.vormstein@cern.ch)  2011-02-01
// based on MUV1Fiber.cc
//
// Simulation of the iron bolts holding MUV1 together
//
//---------------------------------------------------------------------

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

#include "MUV1Bolt.hh"
#include "MUV1SD.hh"
#include "G4SDManager.hh"

/// \class MUV1Bolt
/// \Brief
/// MUV1Bolt class.
/// \EndBrief
///
/// \Detailed
/// This class creates a holding bolt, which is used to fix the layers of the MUV1.
/// \EndDetailed


MUV1Bolt::MUV1Bolt(G4Material * Material,
		     G4LogicalVolume * MotherVolume,
		     G4ThreeVector BoltPosition,
		     G4int iCopy) :
  NA62VComponent(Material,MotherVolume),
  fiCopy        (iCopy),
  fBoltPosition(BoltPosition)	//Position of the fiber
{
  ReadGeometryParameters();
   // Mandatory here to Find or Build the needed materials
  MUV1MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void MUV1Bolt::ReadGeometryParameters()
{  
  MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();
  
  fBoltRadius = GeoPars->GetConnectionHoleRadius();
  fBoltLength = GeoPars-> GetMUV1Length();
  fAirGapWidth = GeoPars->GetAirGapWidth();
}

void MUV1Bolt::CreateGeometry()
{
  G4Tubs * BoltTube = new G4Tubs("MUV1BoltTube",	//Tube with r=fFiverRadius and l=fBoltLength
				  0.,
				  fBoltRadius-0.5*fAirGapWidth,
				  fBoltLength/2.,
				  0.*deg,
				  360.*deg);
  
  fLogicalVolume = new G4LogicalVolume(BoltTube,
				       fMaterial,
				       "MUV1BoltTube",
				       0,
				       0,
				       0 );

  G4RotationMatrix  fBoltRotation(CLHEP::HepRotationX(0.*deg));
  G4Transform3D fBoltTransform = G4Transform3D(fBoltRotation, fBoltPosition);
  
  fPhysicalVolume = new G4PVPlacement(fBoltTransform,
				      fLogicalVolume,
				      "MUV1BoltTube",
				      fMotherVolume,
				      false,
				      fiCopy);

}

void MUV1Bolt::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,0.5,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
