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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------

#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4Box.hh"
#include "SpectrometerGeometryParameters.hh"
#include "SpectrometerMaterialParameters.hh"
#include "SpectrometerMagnet.hh"

SpectrometerMagnet::SpectrometerMagnet
(G4Material* Material, G4LogicalVolume* MotherVolume, G4ThreeVector Position,
 G4double FieldScaleFactor) : NA62VComponent(Material, MotherVolume) {

  ReadGeometryParameters();
  fPosition = Position;
  fFieldScaleFactor = FieldScaleFactor;

  // Mandatory here to Find or Build the needed materials
  SpectrometerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void SpectrometerMagnet::ReadGeometryParameters() {
  // Read all the geometrical parameters and copy them to private members
  SpectrometerGeometryParameters* GeoPars = SpectrometerGeometryParameters::GetInstance();
  fZLength = GeoPars->GetMagnetZLength();
  fXLength = GeoPars->GetMagnetXLength();
  fYLength = GeoPars->GetMagnetYLength();
  fFieldStrength = GeoPars->GetMagnetFieldStrength();
}

void SpectrometerMagnet::CreateGeometry() {
  fSolidVolume    = new G4Box("MNP33Magnet", 0.5*fXLength, 0.5*fYLength, 0.5*fZLength);
  fLogicalVolume  = new G4LogicalVolume(fSolidVolume, fMaterial, "MNP33Magnet", 0, 0, 0);
  fPhysicalVolume = new G4PVPlacement(0, fPosition, fLogicalVolume, "MNP33Magnet", fMotherVolume, false, 0);

  if (fabs(fFieldScaleFactor)>0.001) {
    fMagField = new G4UniformMagField(G4ThreeVector(0.0, fFieldStrength*fFieldScaleFactor, 0.0));
    fFieldMgr = new G4FieldManager(fMagField);
    fFieldMgr->SetDetectorField(fMagField);
    fFieldMgr->CreateChordFinder(fMagField);
    fLogicalVolume->SetFieldManager(fFieldMgr, true);
  }
}

void SpectrometerMagnet::SetProperties() {
  // Set visualization properties
 fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0,0.1));
 fLogicalVolume ->SetVisAttributes(fVisAtt);
}
