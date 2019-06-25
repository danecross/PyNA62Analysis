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
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), 4 March 2014
//
// --------------------------------------------------------------------

/// \class MUV3Plate
/// \Brief
/// Vertical front and back Al plates of MUV3.
/// \EndBrief
/// \Detailed
/// They consist of two hapves with a vertical gap in the middle,
/// and with a hole for the beam pipe.
/// \EndDetailed

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4SubtractionSolid.hh"
#include "globals.hh"

#include "MUV3Plate.hh"
#include "MUV3GeometryParameters.hh"
#include "MUV3MaterialParameters.hh"

MUV3Plate::MUV3Plate(G4Material* Material, G4LogicalVolume* MotherVolume,
		     G4double ZPosition, G4double Thickness, G4String Name) : 
  NA62VComponent(Material,MotherVolume),
  fThickness(Thickness),
  fZPosition(ZPosition),
  fName     (Name),
  fNameL    (fName + "L"),
  fNameR    (fName + "R")
{
  ReadGeometryParameters();
  MUV3MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void MUV3Plate::ReadGeometryParameters() {
  MUV3GeometryParameters* GeoPars = MUV3GeometryParameters::GetInstance();
  fXSize       = GeoPars->GetNModulesX() * GeoPars->GetLargeModuleSize() * 0.5;
  fYSize       = GeoPars->GetNModulesY() * GeoPars->GetLargeModuleSize();
  fInnerRadius = GeoPars->GetPassiveInnerRadius();
  fGapWidth    = GeoPars->GetGapWidth();
}

void MUV3Plate::CreateGeometry() {

  G4Box* PlateSolid = new G4Box("PlateSolid", 0.5*fXSize, 0.5*fYSize, 0.5*fThickness);
  G4Tubs* HoleSolid = new G4Tubs("HoleSolid", 0.0, fInnerRadius, 0.55*fThickness, 0.0, 360*deg);

  G4RotationMatrix *Rotation = new G4RotationMatrix;
  G4ThreeVector Trans(0.5*fXSize, 0.0, 0.0);

  G4SubtractionSolid* SolidL = new G4SubtractionSolid
    ("SolidL", PlateSolid, HoleSolid, Rotation, Trans);
  G4SubtractionSolid* SolidR = new G4SubtractionSolid
    ("SolidR", PlateSolid, HoleSolid, Rotation, -Trans);

  G4LogicalVolume* LogicalL = new G4LogicalVolume(SolidL, fMaterial, fNameL);
  G4LogicalVolume* LogicalR = new G4LogicalVolume(SolidR, fMaterial, fNameR);

  G4ThreeVector PositionL = G4ThreeVector (-0.5*fXSize-0.5*fGapWidth, 0.0, fZPosition);
  G4ThreeVector PositionR = G4ThreeVector (+0.5*fXSize+0.5*fGapWidth, 0.0, fZPosition);

  new G4PVPlacement (0, PositionL, LogicalL, fNameL, fMotherVolume, false, 0);
  new G4PVPlacement (0, PositionR, LogicalR, fNameR, fMotherVolume, false, 0);
}

void MUV3Plate::SetProperties() {}
