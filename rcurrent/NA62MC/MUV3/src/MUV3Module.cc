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
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) March 2014
//
// --------------------------------------------------------------------

/// \class MUV3Module
/// \Brief
/// MUV3 module: scintillator and PMTs belonging to one tile
/// \EndBrief

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4SubtractionSolid.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV3Module.hh"
#include "MUV3PMT.hh"
#include "MUV3GeometryParameters.hh"
#include "MUV3MaterialParameters.hh"

#include "MUV3SD.hh"
#include "G4SDManager.hh"

MUV3Module::MUV3Module
(G4Material* Material, G4LogicalVolume* MotherVolume, G4ThreeVector Position, G4int Type, G4int ModuleID) :
  NA62VComponent(Material,MotherVolume),
  fModuleType      (Type),
  fModuleID        (ModuleID),
  fName            (Form("MUV3Module%03d", fModuleID)),
  fScintillatorName(Form("MUV3Scint%03d",  fModuleID)),
  fPosition        (Position)
{
  ReadGeometryParameters();

  MUV3MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void MUV3Module::ReadGeometryParameters() {
  MUV3GeometryParameters* GeoPars = MUV3GeometryParameters::GetInstance();

  fScintillatorZLength = GeoPars->GetScintillatorZLength();
  fSize = (fModuleType==0) ?
    GeoPars->GetLargeModuleSize() : GeoPars->GetSmallModuleSize();

  if (fModuleType==0) { // large module
    fXPMT1 = GeoPars->GetPMT1LargeX();
    fYPMT1 = GeoPars->GetPMT1LargeY();
    fXPMT2 = GeoPars->GetPMT2LargeX();
    fYPMT2 = GeoPars->GetPMT2LargeY();
  }
  else if (fModuleType==1) { // small square module
    fXPMT1 = GeoPars->GetPMT1CornerX();
    fYPMT1 = GeoPars->GetPMT1CornerY();
    fXPMT2 = GeoPars->GetPMT2CornerX();
    fYPMT2 = GeoPars->GetPMT2CornerY();
  }
  else { // halves of odd-shaped central modules
    fXPMT1 = GeoPars->GetPMTSideX();
    fYPMT1 = GeoPars->GetPMTSideY();
    fXPMT2 = fXPMT1;
    fYPMT2 = fYPMT1;
  }

  fZLength     = GeoPars->GetModuleZLength();
  fInnerRadius = GeoPars->GetActiveInnerRadius();
}

G4VSolid* MUV3Module::ModuleShape(G4String Name, G4double length) {
  G4VSolid *Result;
  
  if (fModuleType==0 || fModuleType==1) { // square modules
    Result = new G4Box(fName, 0.5*fSize, 0.5*fSize, 0.5*length);
  }
  else { // odd-shaped halves of side modules
    G4Box *box   = new G4Box("box", 0.25*fSize, 0.5*fSize, 0.5*length);
    G4Tubs *tube = new G4Tubs("tube", 0, fInnerRadius, 0.55*length, 0.0, 360*deg);
    G4RotationMatrix* rot = new G4RotationMatrix;
    G4ThreeVector trans =
      (fModuleType==2) ?
      G4ThreeVector(+0.25*fSize, -fSize, 0.0) : // left
      G4ThreeVector(-0.25*fSize, -fSize, 0.0);  // right
    Result = new G4SubtractionSolid(Name, box, tube, rot, trans);
  }
  return Result;
}

void MUV3Module::CreateGeometry() {

  //////////////////////////
  // Place the mother volume

  fSolidVolume   = ModuleShape(fName, fZLength);
  fLogicalVolume = new G4LogicalVolume(fSolidVolume, fMaterial, fName);

  G4RotationMatrix* Rotation = new G4RotationMatrix;

  // rotation of corner pads (to get the PMT position correct)
  if (fModuleID==151) Rotation->rotateZ( 90*deg);
  if (fModuleID==146) Rotation->rotateZ(180*deg);
  if (fModuleID==144) Rotation->rotateZ(-90*deg);

  // rotation of the irreguilarly shapes side half-pads
  if (fModuleID==145) Rotation->rotateZ(180*deg);
  if (fModuleID==147) Rotation->rotateZ(-90*deg);
  if (fModuleID==148) Rotation->rotateZ( 90*deg);

  fPhysicalVolume = new G4PVPlacement
    (Rotation, fPosition, fLogicalVolume, fName, fMotherVolume, false, fModuleID);

  //////////////////////////
  // Place the scintillators

  G4VSolid* ScintillatorSolid = ModuleShape(fScintillatorName, fScintillatorZLength);

  G4LogicalVolume* ScintillatorLogical = new
    G4LogicalVolume(ScintillatorSolid, G4Material::GetMaterial("G4_POLYSTYRENE"), fScintillatorName);

  G4ThreeVector ScintillatorPosition =
    G4ThreeVector(0.0, 0.0, 0.5*fScintillatorZLength - 0.5*fZLength);

  new G4PVPlacement
    (0, ScintillatorPosition, ScintillatorLogical,
     fScintillatorName, fLogicalVolume, false, fModuleID);

  //////////////////////////////////////////////
  // Place the PMT windows
  //
  // PMT numbering convention, except modules 145 and 150:
  // (top PMT ID) = (pad ID), (bottom PMT ID) = (pad ID) + 200.
  //
  // PMT numbering convention for modules 145 and 150 (with horizontally aligned PMTs):
  // PMTs 145, 150 are on the Jura side (positive X), PMTs 345 and 350 are on the Saleve side.

  G4int id1 = fModuleID, id2 = fModuleID+200;

  if (fModuleID==144 || fModuleID==146) {
    id2 = fModuleID; id1 = fModuleID+200;
  }

  if (fModuleType==0 || fModuleType==1) { // square pads
    new MUV3PMT(G4Material::GetMaterial("G4_GLASS_PLATE"),
		fLogicalVolume, fXPMT1, fYPMT1, id1);
    new MUV3PMT(G4Material::GetMaterial("G4_GLASS_PLATE"),
		fLogicalVolume, fXPMT2, fYPMT2, id2);
  }
  else { // half of an odd-shaped pad
    G4int id = fModuleID;
    if ((fModuleID==145 && fModuleType==3) ||
	(fModuleID==147 && fModuleType==2) ||
	(fModuleID==148 && fModuleType==3) ||
	(fModuleID==150 && fModuleType==2)) {
      id += 200;
    }
    new MUV3PMT(G4Material::GetMaterial("G4_GLASS_PLATE"), fLogicalVolume, fXPMT1, fYPMT1, id);
  }

  // Define scintillator as a sensitive volume
  G4SDManager* SDManager = G4SDManager::GetSDMpointer();
  G4VSensitiveDetector* Muv3SD = SDManager->FindSensitiveDetector("/MUV3");
  ScintillatorLogical->SetSensitiveDetector(Muv3SD);
}

void MUV3Module::SetProperties() {
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt->SetVisibility(true);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
