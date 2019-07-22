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
// Created by Gia Khoriauli (gia.khoriauli@cern.ch)  2017-11-09
//
// Simulation of the iron outer spacers used for iron bolts holding MUV1
// layers together
//
//---------------------------------------------------------------------

#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV1GeometryParameters.hh"
#include "MUV1MaterialParameters.hh"

#include "MUV1OuterSpacer.hh"

/// \class MUV1OuterSpacer
/// \Brief
/// MUV1OuterSpacer class.
/// \EndBrief
///
/// \Detailed
/// This class creates an outer spacer for a connection bolt, which is used to fix the layers of the MUV1 together.
/// \EndDetailed


MUV1OuterSpacer::MUV1OuterSpacer(G4Material * Material,
		     G4LogicalVolume * MotherVolume,
		     G4ThreeVector OuterSpacerPosition,
		     G4int iCopy) :
  NA62VComponent(Material,MotherVolume),
  fiCopy        (iCopy),
  fOuterSpacerPosition(OuterSpacerPosition)
{

  ReadGeometryParameters();
   // Mandatory here to Find or Build the needed materials
  MUV1MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void MUV1OuterSpacer::ReadGeometryParameters()
{  
  MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();
  
  fOuterSpacerInnerRadius = GeoPars->GetConnectionHoleRadius() + 0.25 * mm;
  fOuterSpacerOuterRadius = GeoPars->GetOuterSpacerOuterRadius();
  fOuterSpacerLength      = GeoPars->GetScintillatorSpacerSize().z();
  fAirGapWidht            = GeoPars->GetAirGapWidth();
}

void MUV1OuterSpacer::CreateGeometry()
{
  G4Tubs * OuterSpacerTube = new G4Tubs("MUV1OuterSpacerTube",
				  fOuterSpacerInnerRadius,
				  fOuterSpacerOuterRadius,
				  fOuterSpacerLength-0.1*fAirGapWidht,
				  0.*deg,
				  360.*deg);
  
  fLogicalVolume = new G4LogicalVolume(OuterSpacerTube,
				       fMaterial,
				       "MUV1OuterSpacerTube",
				       0,
				       0,
				       0 );

  G4RotationMatrix  fOuterSpacerRotation(CLHEP::HepRotationX(0.*deg));
  G4Transform3D fOuterSpacerTransform = G4Transform3D(fOuterSpacerRotation, fOuterSpacerPosition);

  fPhysicalVolume = new G4PVPlacement(fOuterSpacerTransform,
				      fLogicalVolume,
				      "MUV1OuterSpacerTube",
				      fMotherVolume,
				      false,
				      fiCopy);

}

void MUV1OuterSpacer::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,0.1,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
