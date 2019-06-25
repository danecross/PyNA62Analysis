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
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4UnionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "NewCHODGeometryParameters.hh"
#include "NewCHODMaterialParameters.hh"

#include "NewCHODScintillatorCounter.hh"
#include "NewCHODSD.hh"
#include "G4SDManager.hh"

NewCHODScintillatorCounter::NewCHODScintillatorCounter
(G4Material* Material, G4LogicalVolume* MotherVolume, G4ThreeVector Position,
 G4int BrickID, G4int ID) :
  NA62VComponent(Material,MotherVolume),
  fBrickID(BrickID),
  fID     (ID),
  fScintillatorPosition(Position)
{

  ReadGeometryParameters();

  // Mandatory here to find or build the needed materials
  NewCHODMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void NewCHODScintillatorCounter::ReadGeometryParameters() {
  NewCHODGeometryParameters* GeoPars = NewCHODGeometryParameters::GetInstance();
  fScintillatorSize = GeoPars->GetScintSize();
  fInnerRadius      = GeoPars->GetInnerRadius();
  fOuterRadius      = GeoPars->GetOuterRadius();
}

void NewCHODScintillatorCounter::CreateGeometry() {
  G4String Name = Form("NewCHODCounter%03d_%03d", fBrickID, fID);
  G4double OuterRadius = fOuterRadius;
  if (fBrickID%100==7) OuterRadius *= 2.0; // a rectangular edge brick

  G4Box* BoxSolid = new G4Box
    ("BoxSolid", 0.5*fScintillatorSize.x(), 0.5*fScintillatorSize.y(), 0.5*fScintillatorSize.z());
  G4Tubs* RadialSolid = new G4Tubs
    ("RadialSolid", fInnerRadius, OuterRadius, 0.55*fScintillatorSize.z(), 0.0, 360*deg);

  G4RotationMatrix *Rotation = new G4RotationMatrix;
  G4ThreeVector Trans(-fScintillatorPosition.x(), -fScintillatorPosition.y(), 0.0);
  fSolidVolume = new G4IntersectionSolid (Name, BoxSolid, RadialSolid, Rotation, Trans);
  fLogicalVolume = new G4LogicalVolume (fSolidVolume, fMaterial, Name, 0, 0, 0);
  fPhysicalVolume = new G4PVPlacement
    (0, fScintillatorPosition, fLogicalVolume, Name, fMotherVolume, false, fID);
  // last parameter of G4PVPlacement: copy number = channel ID not brick ID

  G4SDManager* SDmanager = G4SDManager::GetSDMpointer();
  G4String NewCHODSDname = "/NewCHOD";
  G4VSensitiveDetector *SD = SDmanager->FindSensitiveDetector(NewCHODSDname);
  fLogicalVolume->SetSensitiveDetector(SD);
}

void NewCHODScintillatorCounter::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt->SetVisibility(true);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
