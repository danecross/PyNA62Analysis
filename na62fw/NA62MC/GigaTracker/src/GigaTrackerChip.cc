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
// Created by Bob Velghe (bob.velghe@cern.ch) 2012-01-09
// (Based on GigaTrackerStation.cc)
// --------------------------------------------------------------
//
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerMaterialParameters.hh"

#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "GigaTrackerChip.hh"

GigaTrackerChip::GigaTrackerChip(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position, G4int iCopy, G4int iStation) : 
  NA62VComponent(Material, MotherVolume),
  fiCopy(iCopy),
  fiStation(iStation),
  fPosition(Position)
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

GigaTrackerChip::~GigaTrackerChip() {}

void GigaTrackerChip::ReadGeometryParameters()
{
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance();
  fXChipLength = GeoPars->GetGigaTrackerChipXLength(0); 
  fYChipLength = GeoPars->GetGigaTrackerChipYLength(0); 
  fZChipLength = GeoPars->GetGigaTrackerChipZLength(fiStation); 
}

void GigaTrackerChip::CreateGeometry()
{
  //////////
  // Chip //
  //////////
  fSolidVolume = new G4Box("GigaTrackerChip",0.5*fXChipLength,0.5*fYChipLength,0.5*fZChipLength);
  fLogicalVolume = new G4LogicalVolume(fSolidVolume,fMaterial,"GigaTrackerChip",0,0,0);
  fPhysicalVolume = new G4PVPlacement(0,
				     fPosition,
				     fLogicalVolume,
				     "GigaTrackerChip",
				     fMotherVolume, //Mother logical volume
				     false,
				     fiCopy);
}

void GigaTrackerChip::SetProperties()
{
  fVisAtt = new G4VisAttributes(G4Colour(0.0,1.0,0.0)); //Green (red,green,blue)
  fVisAtt->SetVisibility(true);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
