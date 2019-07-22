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
#include "G4PVReplica.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "HACGeometryParameters.hh"
#include "HACMaterialParameters.hh"

#include "HACModule.hh"
#include "HACAbsorberLayer.hh"
#include "HACScintillatorLayer.hh"
#include "HACSD.hh"
#include "G4SDManager.hh"

HACModule::HACModule(G4Material * Material, G4LogicalVolume * MotherVolume,
		     G4Transform3D Transform3D, G4int iCopy) : 
  NA62VComponent(Material,MotherVolume),
  fiCopy(iCopy),
  fTransform3D(Transform3D)
{
  ReadGeometryParameters();
  
  // Mandatory here to Find or Build the needed materials
  HACMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

HACModule::~HACModule(){}

void HACModule::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  HACGeometryParameters* GeoPars = HACGeometryParameters::GetInstance();
  fHACModuleXLength = GeoPars->GetHACModuleXLength();
  fHACModuleYLength = GeoPars->GetHACModuleYLength();
  fHACModuleZLength = GeoPars->GetHACModuleZLength();

  fNLayers = GeoPars->GetNLayers();

  fScintillatorLayerZLength = GeoPars->GetScintillatorLayerZLength();
  fAbsorberLayerZLength = GeoPars->GetAbsorberLayerZLength();

}

void HACModule::CreateGeometry()
{

  fSolidVolume = new G4Box("HACModule",
			   fHACModuleXLength/2.,
			   fHACModuleYLength/2.,
			   fHACModuleZLength/2.);
  fLogicalVolume= new G4LogicalVolume(fSolidVolume,                    // solid
				      fMaterial,                       // material
				      "HACModule",                     // name
				      0,                               // field manager 
				      0,                               // sensitive detector
				      0);                              // user limits
  fPhysicalVolume = new G4PVPlacement(fTransform3D,
				      fLogicalVolume,             // its logical volume
				      "HACModule",                // its name
				      fMotherVolume,              // its mother  volume
				      false,                      // no boolean operations
				      fiCopy);                    // copy number
  G4RotationMatrix Rotation;
  G4ThreeVector Position;
  
  for(G4int iLayer=0; iLayer < fNLayers-1; iLayer++) {
    int ichannel = fiCopy*10+(int)(iLayer/6);
    G4double zscint = -fHACModuleZLength/2. + 
      (G4double)iLayer*(fScintillatorLayerZLength+fAbsorberLayerZLength) + 
      fScintillatorLayerZLength/2.;
    Position = G4ThreeVector(0.,0.,zscint);
    G4Transform3D ScintillatorTransform3D = G4Transform3D(Rotation,Position);
    new HACScintillatorLayer(G4Material::GetMaterial("G4_POLYSTYRENE"),
			     fLogicalVolume,
			     ScintillatorTransform3D,
			     ichannel);
    Position += G4ThreeVector(0.,0.,fAbsorberLayerZLength/2.+fScintillatorLayerZLength/2.);
    G4Transform3D AbsorberTransform3D = G4Transform3D(Rotation,Position);
    new HACAbsorberLayer(G4Material::GetMaterial("G4_Pb"),
			 fLogicalVolume,
			 AbsorberTransform3D,
			 ichannel);    
  }
  /*
  Position += G4ThreeVector(0.,0.,fScintillatorLayerZLength/2.+fAbsorberLayerZLength/2.);
  G4Transform3D ScintillatorTransform3D = G4Transform3D(Rotation,Position);
  new HACScintillatorLayer(G4Material::GetMaterial("G4_POLYSTYRENE"),
			   fLogicalVolume,
			   ScintillatorTransform3D,
			   fiCopy);
  */
}

void HACModule::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}