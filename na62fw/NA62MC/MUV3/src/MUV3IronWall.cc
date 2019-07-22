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
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), 4 March 2014
//
// --------------------------------------------------------------------

/// \class MUV3IronWall
/// \Brief
/// Iron wall in front of MUV3
/// \EndBrief

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4SubtractionSolid.hh"
#include "globals.hh"

#include "MUV3IronWall.hh"
#include "MUV3GeometryParameters.hh"
#include "MUV3MaterialParameters.hh"

MUV3IronWall::MUV3IronWall(G4Material* Material, G4LogicalVolume* MotherVolume, G4ThreeVector Position) : 
  NA62VComponent(Material,MotherVolume),
  fPosition(Position),
  fName    (Form("MUV3FeWall"))
{
  ReadGeometryParameters();
  MUV3MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void MUV3IronWall::ReadGeometryParameters() {
  MUV3GeometryParameters* GeoPars = MUV3GeometryParameters::GetInstance();
  fXSize       = GeoPars->GetFeWallXSize();
  fYSize       = GeoPars->GetFeWallYSize();
  fThickness   = GeoPars->GetFeWallThickness();
  fInnerRadius = GeoPars->GetFeWallInnerRadius();
}

void MUV3IronWall::CreateGeometry() {

  G4Box* WallSolid = new
    G4Box("WallSolid", 0.5*fXSize, 0.5*fYSize, 0.5*fThickness);
  G4Tubs* HoleSolid = new
    G4Tubs("HoleSolid", 0.0, fInnerRadius, 0.55*fThickness, 0.0, 360*deg);
  G4SubtractionSolid* Result = new
    G4SubtractionSolid(fName, WallSolid, HoleSolid);

  fLogicalVolume = new G4LogicalVolume(Result, fMaterial, fName);

  fPhysicalVolume = new G4PVPlacement
    (0, fPosition, fLogicalVolume, fName, fMotherVolume, false, 0);
}

void MUV3IronWall::SetProperties() {}
