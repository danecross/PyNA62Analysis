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
// Created by Bob Velghe (bob.velghe@cern.ch) 2012-01-16
// (Based on GigaTrackerStation.cc)
// --------------------------------------------------------------
//

#include "GigaTrackerBumpBonds.hh"

GigaTrackerBumpBonds::GigaTrackerBumpBonds(G4Material* Material, G4LogicalVolume* MotherVolume, G4ThreeVector Position, G4int iCopy) :
  NA62VComponent(Material, MotherVolume),
  fiCopy(iCopy),
  fPosition(Position)
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

void GigaTrackerBumpBonds::ReadGeometryParameters()
{
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance();
  fBumpBondingRLength = GeoPars->GetGigaTrackerBumpBondingRLength();
  fBumpBondingZLength = GeoPars->GetGigaTrackerBumpBondingZLength();
  fSensorXLength = GeoPars->GetGigaTrackerSensorXLength(0);
  fSensorYLength = GeoPars->GetGigaTrackerSensorYLength(0);
  fNumberOfPixels = GeoPars->GetGigaTrackerNumberOfPixels();
}

void GigaTrackerBumpBonds::CreateGeometry()
{

  //Rem: Replica or parameterised volume must be the only daughter of a volume
    
  ///////////////////////////
  // Solder bumps envelope //
  ///////////////////////////
  fSolidVolume = new G4Box("GigaTrackerBumpBonds",0.5*fSensorXLength,0.5*fSensorYLength,0.5*fBumpBondingZLength);
  fLogicalVolume = new G4LogicalVolume(fSolidVolume,fMaterial,"GigaTrackerBumpBonds",0,0,0);
  fPhysicalVolume = new G4PVPlacement(0, // rotation matrix
				      fPosition, // its position
				      fLogicalVolume,       // its logical volume
				      "GigaTrackerBumpBonds", // its name
				      fMotherVolume,        // its mother  volume
				      false,                // no boolean operations
				      fiCopy);              // copy number

  ///////////////
  // Bump bond //
  //////////////
  
  fBumpBondingParam = new GigaTrackerBumpBondingParameterisation();
  fBumpBondSolidVolume = new G4Tubs("GigaTrackerBumpBond", 0., fBumpBondingRLength, 0.5 * fBumpBondingZLength, 0., 360 *deg);
  fBumpBondLogicalVolume = new G4LogicalVolume(fBumpBondSolidVolume, G4Material::GetMaterial("GTK_BumpBonding"), "GigaTrackerBumpBond", 0, 0, 0);
  fBumpBondPhysicalVolume = new G4PVParameterised("GigaTrackerBumpBond",
						  fBumpBondLogicalVolume,     // logical volume
						  fLogicalVolume,             // mother volume
						  kUndefined,                 // axis
						  fNumberOfPixels,                      // replicas
						  fBumpBondingParam);         // G4VPVParameterisation
}

void GigaTrackerBumpBonds::SetProperties()
{
  fVisAtt = new G4VisAttributes(G4Colour(0.0,0.0,1.0)); //Blue (red,green,blue)
  fVisAtt->SetVisibility(true);
  fBumpBondLogicalVolume->SetVisAttributes(fVisAtt);
}
