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
// Created by Vito Palladino (Vito.Palladino@cern.ch) 2008-12-18
//
// --------------------------------------------------------------
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"
#include "G4RotationMatrix.hh"
#include "G4ThreeVector.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "CHANTIGeometryParameters.hh"
#include "CHANTIMaterialParameters.hh"

#include "CHANTIFiber.hh"

#include "CHANTISD.hh"
#include "G4SDManager.hh"




CHANTIFiber::CHANTIFiber(G4Material* Material, 
		       G4LogicalVolume* MotherVolume, 
		       G4ThreeVector Position, 
		       G4RotationMatrix* Rotation,
		       G4int NofCopy,
		       G4double HalfLength) :
  NA62VComponent(Material, MotherVolume),
  fHalfLength(HalfLength),
  fNofCopy(NofCopy),
  fPosition(Position),
  fRotationMatrix(Rotation)
{

  // storing the construction field in my variables

  ReadGeometryParameters();
  
  // Mandatory here to Find or Build the needed materials
  CHANTIMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
  
}


CHANTIFiber::~CHANTIFiber(){}


void CHANTIFiber::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  CHANTIGeometryParameters* GeoPars = CHANTIGeometryParameters::GetInstance();

  fFiberRadius = GeoPars->GetCHANTIFiberRadius(); 
  
  fCHANTISensitiveDetectorName = GeoPars->GetCHANTISensitiveDetectorName();

  fFiberRadius = GeoPars->GetCHANTIFiberRadius();;                         // Rmax 

}

void CHANTIFiber::CreateGeometry()
{


  // Sensitive detector: in general it would be associated to smaller volume/s inside the global box/es
 
  //G4SDManager* SDMan = G4SDManager::GetSDMpointer();
  //G4VSensitiveDetector* FiberSD = SDMan->FindSensitiveDetector(fCHANTISensitiveDetectorName);
  //CHANTISD * FiberSD = new CHANTISD(fCHANTISensitiveDetectorName, fCHANTICollectionName);
  //SDman->AddNewDetector(FiberSD);

  // Build the Single tile 



  fFiberSolidVolume = new G4Tubs("Fiber", 
				 0.,                                    // Rmin1
				 fFiberRadius,                         // Rmax
				 fHalfLength,                         // pDz
				 0.*deg,                             // Fimin
				 360.*deg);                         // Fimax
  
  fFiberLogicalVolume = new G4LogicalVolume(fFiberSolidVolume,            // solid
					    fMaterial,                   // material
					    "Fiber",                    // name
					    0,                         // field manager 
					    0,                        // sensitive detector
					    0);                      // user limits
  
  fFiberPhysicalVolume = new G4PVPlacement(fRotationMatrix,                  // its Rotation Matrix 
					   fPosition,                        // its position
					   fFiberLogicalVolume,             // its logical volume
					   "Fiber",                        // its name
					   fMotherVolume,                 // its mother volume
					   false,                        // no boolean operations
					   fNofCopy,                    // copies  
					   false);                     // volumes intersection check disabled 
  


  // add sensitive volumes
  //  fFiberLogicalVolume->SetSensitiveDetector(FiberSD);

}

void CHANTIFiber::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(.0, 0.5, 0.5));
  fVisAtt -> SetVisibility(false);
  fFiberLogicalVolume ->SetVisAttributes(fVisAtt);
}
