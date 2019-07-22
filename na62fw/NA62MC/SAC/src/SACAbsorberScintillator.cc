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
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "SACGeometryParameters.hh"
#include "SACMaterialParameters.hh"

#include "SACAbsorberScintillator.hh"
#include "SACAbsorberLayer.hh"
#include "SACScintillatorLayer.hh"
#include "SACSD.hh"
#include "G4SDManager.hh"

SACAbsorberScintillator::SACAbsorberScintillator(G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();
  
  // Mandatory here to Find or Build the needed materials
  SACMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

SACAbsorberScintillator::~SACAbsorberScintillator(){}

void SACAbsorberScintillator::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  SACGeometryParameters* GeoPars = SACGeometryParameters::GetInstance();

  fSACSimulationMode = GeoPars->GetSACSimulationMode();
  
  fSACDetectorXLength = GeoPars->GetSACDetectorXLength();
  fSACDetectorYLength = GeoPars->GetSACDetectorYLength();
  fScintillatorLayerZLength = GeoPars->GetScintillatorLayerZLength();
  fAbsorberLayerZLength = GeoPars->GetAbsorberLayerZLength();
  fFiberSpacing = GeoPars->GetFiberSpacing();
}

void SACAbsorberScintillator::CreateGeometry() {

  // Define AbsorberScintillator Box
  if(fSACSimulationMode==2)
    fSolidVolume = new G4Box("SACAbsorberScintillator",
			     fFiberSpacing/2.,
			     fFiberSpacing/2.,
			     fScintillatorLayerZLength/2.+fAbsorberLayerZLength/2.);
  else
    fSolidVolume = new G4Box("SACAbsorberScintillator",
			     fSACDetectorXLength/2.,
			     fSACDetectorYLength/2.,
			     (fAbsorberLayerZLength+fScintillatorLayerZLength)/2.);
  fLogicalVolume= new G4LogicalVolume(fSolidVolume,                    // solid
				      fMaterial,                       // material
				      "SACAbsorberScintillator",       // name
				      0,                               // field manager 
				      0,                               // sensitive detector
				      0);                              // user limits
  // Put 1 Absorber layer and 1 Scintillator layer inside
  G4RotationMatrix ZeroRotation;
  G4ThreeVector ScintillatorPosition    = G4ThreeVector(0.,0., fScintillatorLayerZLength/2.);
  G4Transform3D ScintillatorTransform3D = G4Transform3D(ZeroRotation, ScintillatorPosition);
  new SACScintillatorLayer(G4Material::GetMaterial("G4_POLYSTYRENE"),
			   fLogicalVolume,
			   ScintillatorTransform3D,
			   0);   
  G4ThreeVector AbsorberPosition        = G4ThreeVector(0.,0.,-fAbsorberLayerZLength/2.);
  G4Transform3D AbsorberTransform3D     = G4Transform3D(ZeroRotation, AbsorberPosition);
  new SACAbsorberLayer(G4Material::GetMaterial("SAC_PbSnAlloy"),
		       fLogicalVolume,
		       AbsorberTransform3D,
		       0);
}

void SACAbsorberScintillator::SetProperties() {
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
