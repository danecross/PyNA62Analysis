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
// Created by Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "HACGeometryParameters.hh"
#include "HACMaterialParameters.hh"

#include "HACScintillatorLayer.hh"
#include "HACSD.hh"
#include "G4SDManager.hh"


HACScintillatorLayer::HACScintillatorLayer(G4Material * Material, G4LogicalVolume * MotherVolume, 
				   G4Transform3D Transform3D, G4int iCopy) : 
NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();
  fTransform3D = Transform3D;
  fiCopy = iCopy;

  // Mandatory here to Find or Build the needed materials
  HACMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

HACScintillatorLayer::~HACScintillatorLayer(){}

void HACScintillatorLayer::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  HACGeometryParameters* GeoPars = HACGeometryParameters::GetInstance();

  fScintillatorLayerXLength = GeoPars->GetScintillatorLayerXLength();
  fScintillatorLayerYLength = GeoPars->GetScintillatorLayerYLength();
  fScintillatorLayerZLength = GeoPars->GetScintillatorLayerZLength();

}

void HACScintillatorLayer::CreateGeometry()
{
  fSolidVolume = new G4Box("HACScintillatorLayer",
			   fScintillatorLayerXLength/2.,
			   fScintillatorLayerYLength/2.,
			   fScintillatorLayerZLength/2.);
  fLogicalVolume= new G4LogicalVolume(fSolidVolume,                        // solid
				      fMaterial,                           // material
				      "HACScintillatorLayer",              // name
				      0,                                   // field manager 
				      0,                                   // sensitive detector
				      0);                                  // user limits
  fPhysicalVolume = new G4PVPlacement(fTransform3D,
				      fLogicalVolume,                 // its logical volume
				      "HACScintillatorLayer",         // its name
				      fMotherVolume,                  // its mother  volume
				      false,                          // no boolean operations
				      fiCopy);                        // copy number


  G4SDManager* SDman = G4SDManager::GetSDMpointer();
  G4String HACSDname = "/HAC";
  G4VSensitiveDetector * HacSD = SDman->FindSensitiveDetector(HACSDname);
  fLogicalVolume->SetSensitiveDetector(HacSD);
}

void HACScintillatorLayer::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
