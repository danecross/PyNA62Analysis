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
// 2012-01-09 Bob Velghe (bob.velghe@cern.ch)
// - Refactoring: Two main sub-classes
//		- GigaTrackerSensorAssembly (pixels, bump bonds, readout chips and glue layer)
//		- GigaTrackerCoolingPlate
//
// 2008-05-15 S.Bifani
// - Fixed a bug in the pixel counting
//
// 2008-04-29 S.Bifani
// - Changed the sensitive detector (pixel -> sensor)
//
// Created by Simone Bifani (Simone.Bifani@cern.ch) 2008-04-22
// --------------------------------------------------------------
//
#include "globals.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerMaterialParameters.hh"
#include "GigaTrackerStation.hh"
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4VisAttributes.hh"
#include "G4Material.hh"

GigaTrackerStation::GigaTrackerStation
(G4Material* Material, G4LogicalVolume* MotherVolume, G4ThreeVector Position, G4int iCopy) :
  NA62VComponent(Material, MotherVolume), fPosition(Position), fiCopy(iCopy) {
  ReadGeometryParameters();
  // Mandatory here to Find or Build the needed materials
  GigaTrackerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

GigaTrackerStation::~GigaTrackerStation() {}

void GigaTrackerStation::ReadGeometryParameters() {

  // Read all the geometrical parameters and copy them to private members
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance();

  fXLength = GeoPars->GetGigaTrackerStationXLength(0);
  fYLength = GeoPars->GetGigaTrackerStationYLength(0);
  fZLength = GeoPars->GetGigaTrackerStationZLength(0);
  fPCBXLength = GeoPars->GetGigaTrackerPCBXLength();
  fPCBHoleXOffset = GeoPars->GetGigaTrackerPCBHoleXOffset();
  fPCBHoleXLength = GeoPars->GetGigaTrackerPCBHoleXLength();
}

void GigaTrackerStation::CreateGeometry() {

  G4double HalfXLength = 0.5 * fXLength;
  G4double HalfYLength = 0.5 * fYLength;
  G4double HalfZLength = 0.5 * fZLength;

  fSolidVolume = new G4Box(Form("GigaTrackerStation%d", fiCopy+1), HalfXLength, HalfYLength, HalfZLength);

  fLogicalVolume = new G4LogicalVolume
    (fSolidVolume, fMaterial, Form("GigaTrackerStation%d", fiCopy+1),
     0,          // field manager 
     0,          // sensitive detector
     0);         // user limits

  fPhysicalVolume = new G4PVPlacement
    (0,	fPosition, fLogicalVolume, Form("GigaTrackerStation%d", fiCopy+1),
     fMotherVolume, false, fiCopy);

  G4double pos = -((fPCBHoleXOffset + 0.5*fPCBHoleXLength)  - 0.5*fPCBXLength);
  fGigaTrackerPCBModule = new GigaTrackerPCBModule
    (G4Material::GetMaterial("G4_Galactic"), fLogicalVolume, G4ThreeVector(pos,0,0), fiCopy);
}

void GigaTrackerStation::SetProperties() {
  fLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible);
}
