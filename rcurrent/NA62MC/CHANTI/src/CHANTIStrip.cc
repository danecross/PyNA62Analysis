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
#include "G4Trd.hh"
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

#include "CHANTIStrip.hh"
#include "CHANTIFiber.hh"

#include "CHANTISD.hh"
#include "G4SDManager.hh"




CHANTIStrip::CHANTIStrip(G4Material* Material, 
		     G4LogicalVolume* MotherVolume, 
		     G4ThreeVector Position, 
		     G4RotationMatrix* Rotation,
		     G4double HalfLength,
		     G4int NofCopy) :
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


CHANTIStrip::~CHANTIStrip(){}


void CHANTIStrip::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  CHANTIGeometryParameters* GeoPars = CHANTIGeometryParameters::GetInstance();

  fTriangleAltitude = GeoPars->GetCHANTITriangleAltitude();
  fTriangleBase     = GeoPars->GetCHANTITriangleBase();
  
  fCHANTISensitiveDetectorName = GeoPars->GetCHANTISensitiveDetectorName();
   

}

void CHANTIStrip::CreateGeometry()
{


  // Sensitive detector: in general it would be associated to smaller volume/s inside the global box/es
 
  G4SDManager* SDMan = G4SDManager::GetSDMpointer();
  G4VSensitiveDetector* StripSD = SDMan->FindSensitiveDetector(fCHANTISensitiveDetectorName);

  //CHANTISD * StripSD = new CHANTISD(fCHANTISensitiveDetectorName, fCHANTICollectionName);
  //SDman->AddNewDetector(StripSD);

  // Build the Single tile 

  G4double HalfTriangleAltitude = fTriangleAltitude/2. ;
  G4double HalfTriangleBase = fTriangleBase/2.     ;
  
  // decrease the triangle dimension to avoid geometrical conflicts in the stations
  // A. Shaikhiev 23 Oct. 2018
  fStripSolidVolume = new G4Trd("Strip", 
			       HalfTriangleBase-0.001*mm,      //dx1
			       0.,                             //dx2
			       fHalfLength,                    //dy1
			       fHalfLength,                    //dy2
			       HalfTriangleAltitude);          //dz
   
  fStripLogicalVolume = new G4LogicalVolume(fStripSolidVolume,           // solid
					   fMaterial,                   // material
					   "Strip",                    // name
					   0,                         // field manager 
					   0,                        // sensitive detector
					   0);                      // user limits
  
  fStripPhysicalVolume = new G4PVPlacement(fRotationMatrix,                  // its Rotation Matrix 
					  fPosition,                        // its position
					  fStripLogicalVolume,             // its logical volume
					  "Strip",                        // its name
					  fMotherVolume,                 // its mother volume
					  false,                        // no boolean operations
					  fNofCopy);                   // copies  
					  
					            
  G4RotationMatrix* Rotation = new G4RotationMatrix();
  Rotation->rotateX(90.*deg);



  CHANTIFiber(G4Material::GetMaterial("G4_POLYACRYLONITRILE"),
	     fStripLogicalVolume,
	     G4ThreeVector(0.,0.,0.),
	     Rotation,
	     fNofCopy,
	     fHalfLength);
  
  // add sensitive volumes
  fStripLogicalVolume->SetSensitiveDetector(StripSD);

}

void CHANTIStrip::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(.0,1.0,.0));
  fVisAtt -> SetVisibility(true);
  fStripLogicalVolume ->SetVisAttributes(fVisAtt);
}
