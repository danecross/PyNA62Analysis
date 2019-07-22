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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 
//	      Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4PVReplica.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "SACGeometryParameters.hh"
#include "SACMaterialParameters.hh"

#include "SACSegment.hh"
#include "SACAbsorberScintillator.hh"
#include "SACSD.hh"
#include "G4SDManager.hh"

SACSegment::SACSegment(G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();
  
  // Mandatory here to Find or Build the needed materials
  SACMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

SACSegment::~SACSegment(){}

void SACSegment::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  SACGeometryParameters* GeoPars = SACGeometryParameters::GetInstance();

  fSACSimulationMode = GeoPars->GetSACSimulationMode();

  fSACDetectorXLength = GeoPars->GetSACDetectorXLength();
  fSACDetectorYLength = GeoPars->GetSACDetectorYLength();

  fAbsorberLayerZLength = GeoPars->GetAbsorberLayerZLength();
  fScintillatorLayerZLength = GeoPars->GetScintillatorLayerZLength();

  fAluminiumLayerXLength = GeoPars->GetAluminiumLayerXLength();
  fAluminiumLayerYLength = GeoPars->GetAluminiumLayerYLength();
  fAluminiumLayerZLength = GeoPars->GetAluminiumLayerZLength();

  fFiberSpacing = GeoPars->GetFiberSpacing();
  fNLayers = GeoPars->GetNLayers();
  
  fLayerSpacing = fAbsorberLayerZLength+fScintillatorLayerZLength;
  fSACAbsorberScintillatorTotalZLength = fNLayers*fLayerSpacing;
}

void SACSegment::CreateGeometry() {
  // Default size (simulation mode 1)
  G4Box* AbsorberScintillatorTotalSolid = new G4Box("AbsorberScintillatorTotal",
						    fSACDetectorXLength/2.,
						    fSACDetectorYLength/2.,
						    fSACAbsorberScintillatorTotalZLength/2.);

  G4Box* AluminiumNoFiberSolid = new G4Box("AluminiumNoFiber",
					   fAluminiumLayerXLength/2.,
					   fAluminiumLayerYLength/2.,
					   fAluminiumLayerZLength/2.);
  // Redefine if simulation in mode 2
  if(fSACSimulationMode==2) {
    AbsorberScintillatorTotalSolid->SetXHalfLength(fFiberSpacing/2);
    AbsorberScintillatorTotalSolid->SetYHalfLength(fFiberSpacing/2);
    AluminiumNoFiberSolid->SetXHalfLength(fFiberSpacing/2.);
    AluminiumNoFiberSolid->SetYHalfLength(fFiberSpacing/2.);
    fSolidVolume = new G4Box("SACSegment",
			     fFiberSpacing/2.,
			     fFiberSpacing/2.,
			     fSACAbsorberScintillatorTotalZLength/2.+ fAluminiumLayerZLength);
  } else {
    fSolidVolume = new G4Box("SACSegment",
			     fSACDetectorXLength/2.,
			     fSACDetectorYLength/2.,
			     fSACAbsorberScintillatorTotalZLength/2.+ fAluminiumLayerZLength);
  }
  G4LogicalVolume* AbsorberScintillatorTotalLogical = new G4LogicalVolume(AbsorberScintillatorTotalSolid,
									  G4Material::GetMaterial("G4_Galactic"),
									  "AbsorberScintillatorTotal");
  G4LogicalVolume* AluminiumNoFiberLogical = new G4LogicalVolume(AluminiumNoFiberSolid,
								 G4Material::GetMaterial("G4_Al"),
								 "AluminiumNoFiber");
  fLogicalVolume= new G4LogicalVolume(fSolidVolume,                    // solid
				      fMaterial,                       // material
				      "SACSegment",       // name
				      0,                               // field manager 
				      0,                               // sensitive detector
				      0);                              // user limits
  // Define 1 Aborber + 1 Scintillator layer
  SACAbsorberScintillator* AbsorberScintillator = new SACAbsorberScintillator(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume);
  // Replicate it in a box
  new G4PVReplica("AbsorberScintillator",
		  AbsorberScintillator->GetLogicalVolume(),
		  AbsorberScintillatorTotalLogical,
		  kZAxis,
		  fNLayers,
		  fAbsorberLayerZLength+fScintillatorLayerZLength,
		  0);
  // Fill SAC segment with Al plates and AbsorberScintillator in SACSegment
  G4ThreeVector AluminiumFrontPosition = G4ThreeVector(0.,0., -fSACAbsorberScintillatorTotalZLength/2.-fAluminiumLayerZLength/2.);
  new G4PVPlacement(0,
		    AluminiumFrontPosition,
		    AluminiumNoFiberLogical,
		    "AluminiumFront",
		    fLogicalVolume,
		    false,
		    0);
  G4ThreeVector AbsorberScintillatorTotalPosition = G4ThreeVector(0.,0.,0.);
  new G4PVPlacement(0,
		    AbsorberScintillatorTotalPosition,
		    AbsorberScintillatorTotalLogical,
		    "AbsorberScintillatorTotal",
		    fLogicalVolume,
		    false,
		    0);
  G4ThreeVector AluminiumBackPosition = G4ThreeVector(0.,0., fSACAbsorberScintillatorTotalZLength/2.+fAluminiumLayerZLength/2.);
  new G4PVPlacement(0,
		    AluminiumBackPosition,
		    AluminiumNoFiberLogical,
		    "AluminiumBack",
		    fLogicalVolume,
		    false,
		    0);
}

void SACSegment::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
