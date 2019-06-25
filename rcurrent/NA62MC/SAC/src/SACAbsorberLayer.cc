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
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "SACGeometryParameters.hh"
#include "SACMaterialParameters.hh"

#include "SACAbsorberLayer.hh"
#include "SACSD.hh"
#include "G4SDManager.hh"


SACAbsorberLayer::SACAbsorberLayer(G4Material * Material, G4LogicalVolume * MotherVolume, 
				   G4Transform3D Transform3D, G4int iCopy) : 
NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();
  fTransform3D = Transform3D;

  fiCopy = iCopy;

  // Mandatory here to Find or Build the needed materials
  SACMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

SACAbsorberLayer::~SACAbsorberLayer(){}

void SACAbsorberLayer::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  SACGeometryParameters* GeoPars = SACGeometryParameters::GetInstance();

  fSACSimulationMode = GeoPars->GetSACSimulationMode();
  
  fAbsorberLayerXLength = GeoPars->GetAbsorberLayerXLength();
  fAbsorberLayerYLength = GeoPars->GetAbsorberLayerYLength();
  fAbsorberLayerZLength = GeoPars->GetAbsorberLayerZLength();

  fFiberDiameter = GeoPars->GetFiberDiameter();
  fFiberSpacing = GeoPars->GetFiberSpacing();
  fNFibers = GeoPars->GetNFibers();
}

void SACAbsorberLayer::CreateGeometry() {
  // Build one or more boxes that will contain all the 
  // detector sections, up to fill the responsibility region
  if(fSACSimulationMode==1) {
    fSolidVolume = new G4Box("SACAbsorberLayer",
			     fAbsorberLayerXLength/2.,
			     fAbsorberLayerYLength/2.,
			     fAbsorberLayerZLength/2.);
    fLogicalVolume= new G4LogicalVolume(fSolidVolume,                        // solid
					fMaterial,                           // material
					"SACAbsorberLayer",              // name
					0,                                   // field manager 
					0,                                   // sensitive detector
					0);                                  // user limits
    fPhysicalVolume = new G4PVPlacement(fTransform3D,
					fLogicalVolume,                 // its logical volume
					"SACAbsorberLayer",         // its name
					fMotherVolume,                  // its mother  volume
					false,                          // no boolean operations
					fiCopy);                        // copy number
    G4Tubs* FiberAbsorberSolid = new G4Tubs("FiberAbsorber",
					    0.,
					    fFiberDiameter/2.,
					    fAbsorberLayerZLength/2.,
					    0.,
					    360*deg);
    G4LogicalVolume* FiberAbsorberLogical = new G4LogicalVolume(FiberAbsorberSolid,
								G4Material::GetMaterial("G4_POLYSTYRENE"),
								"FiberAbsorber");
   for(G4int iFiber=0;iFiber<fNFibers;iFiber++) {
      G4double xCenter = (G4double)iFiber*fFiberSpacing + fFiberSpacing/2. - fFiberSpacing*(G4double)fNFibers/2.;
      for(G4int jFiber=0;jFiber<fNFibers;jFiber++) {
	G4double yCenter = (G4double)jFiber*fFiberSpacing + fFiberSpacing/2. - fFiberSpacing*(G4double)fNFibers/2.;
	G4ThreeVector FiberPosition = G4ThreeVector(xCenter,yCenter,0.);
	new G4PVPlacement(0,
			  FiberPosition,
			  FiberAbsorberLogical,
			  "FiberAbsorber",
			  fLogicalVolume,
			  false,
			  fNFibers*iFiber+jFiber);
      }
    }
  }

  if(fSACSimulationMode==2) {
    fSolidVolume = new G4Box("SACAbsorberLayer",
			     fFiberSpacing/2.,
			     fFiberSpacing/2.,
			     fAbsorberLayerZLength/2.);
    fLogicalVolume= new G4LogicalVolume(fSolidVolume,                        // solid
					fMaterial,                           // material
					"SACAbsorberLayer",              // name
					0,                                   // field manager 
					0,                                   // sensitive detector
					0);                                  // user limits
    fPhysicalVolume = new G4PVPlacement(fTransform3D,
					fLogicalVolume,                 // its logical volume
					"SACAbsorberLayer",         // its name
					fMotherVolume,                  // its mother  volume
					false,                          // no boolean operations
					fiCopy);                        // copy number
    G4Tubs* FiberAbsorberSolid = new G4Tubs("FiberAbsorber",
						0.,
						fFiberDiameter/2.,
						fAbsorberLayerZLength/2.,
						0.,
						360*deg);
    G4LogicalVolume* FiberAbsorberLogical = new G4LogicalVolume(FiberAbsorberSolid,
								G4Material::GetMaterial("G4_POLYSTYRENE"),
								"FiberAbsorber");
    G4ThreeVector FiberPosition     = G4ThreeVector(0.,0.,0.);
    new G4PVPlacement(0,
		      FiberPosition,
		      FiberAbsorberLogical,
		      "FiberAbsorber",
		      fLogicalVolume,
		      false,
		      0);
  }

}

void SACAbsorberLayer::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
