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
// Created by Gia Khoriauli (gia.khoriauli@cern.ch)  2017-11-17
//
// Simulation of the inner iron tube in the central hole of MUV2
//
//-----------------------------------------------------------------------

#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV2GeometryParameters.hh"
#include "MUV2MaterialParameters.hh"

#include "MUV2IronTube.hh"
/// \class MUV2IronTube
/// \Brief
/// MUV2IronTube class.
/// \EndBrief
///
/// \Detailed
/// This class creates an iron tube, which is used for the stability of the detector sandwich-like structure.
//  The tube is placed in the central hole of MUV2 along it whole length.
/// \EndDetailed


MUV2IronTube::MUV2IronTube(	G4Material * Material,
		     	 	 	 	 	 	 	 	 	G4LogicalVolume * MotherVolume,
		     	 	 	 	 	 	 	 	 	G4double TubeLength,
		     	 	 	 	 	 	 	 	 	G4ThreeVector TubePosition,
		     	 	 	 	 	 	 	 	 	G4int iCopy) :
  NA62VComponent(Material,MotherVolume),
  fiCopy       (iCopy),
  fTubeLength  (TubeLength),
  fTubePosition(TubePosition)	//Position of the fiber
{
  ReadGeometryParameters();
   // Mandatory here to Find or Build the needed materials
  MUV2MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void MUV2IronTube::ReadGeometryParameters()
{  
  MUV2GeometryParameters* GeoPars = MUV2GeometryParameters::GetInstance();
  
  fTubeInnerRadius = 0.5 * GeoPars->GetInnerTubeDiameter();
  fTubeOuterRadius = 0.5 * GeoPars->GetHoleDiameter();
  fAirGapWidht     = GeoPars->GetAirGapWidth();
}

void MUV2IronTube::CreateGeometry()
{
  G4Tubs * TransportationTube = new G4Tubs("MUV2IronTubeSV",
				       fTubeInnerRadius,
				       fTubeOuterRadius-0.04*fAirGapWidht,//keep this synchronized (means smaller) with the radius of the central hole of the MUV2ScintillatorLayer
				       fTubeLength/2.,
				       0.*deg,
				       360.*deg);
  
  fLogicalVolume = new G4LogicalVolume(TransportationTube,
				       fMaterial,
				       "MUV2IronTube",
				       0,
				       0,
				       0 );

  G4RotationMatrix  fTubeRotation(CLHEP::HepRotationX(0.*deg));
  G4Transform3D fTubeTransform = G4Transform3D(fTubeRotation, fTubePosition);
  
  fPhysicalVolume = new G4PVPlacement(fTubeTransform,
				      fLogicalVolume,
				      "MUV2IronTubePV",
				      fMotherVolume,
				      false,
				      fiCopy);

}

void MUV2IronTube::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,0.1,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
