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
// Created by Bob Velghe (bob.velghe@cern.ch) 2012-03-14
// --------------------------------------------------------------
//
#ifndef GigaTrackerPCBModule_H
#define GigaTrackerPCBModule_H 1

#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"

#include "NA62VComponent.hh"
#include "GigaTrackerGeometryParameters.hh"

#include "GigaTrackerSensorAssembly.hh"
#include "GigaTrackerCoolingPlate.hh"

class GigaTrackerPCBModule : public NA62VComponent
{

public:
  
  ~GigaTrackerPCBModule();
  GigaTrackerPCBModule(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
private:
  G4int fiCopy;
  G4ThreeVector fPosition;

  G4double fPCBXLength;
  G4double fPCBYLength;
  G4double fPCBZLength;
  
  G4double fPCBHoleXLength;
  G4double fPCBHoleYLength;
  G4double fPCBHoleXOffset;
	
  G4double fSensorAssemblyZLength;
  G4double fCoolingPlateZLength;
  G4double fCoolingPlateTopDepth;
  
  G4Box * fPCBLeftSolidVolume;
  G4LogicalVolume * fPCBLeftLogicalVolume;
  
  G4Box * fPCBRightSolidVolume;
  G4LogicalVolume * fPCBRightLogicalVolume;
  
  G4Box * fPCBTopBottomSolidVolume;
  G4LogicalVolume * fPCBTopBottomLogicalVolume;
  
  G4PVPlacement * fPCBLeftPhysicalVolume;
  G4PVPlacement * fPCBRightPhysicalVolume;
  G4PVPlacement * fPCBTopPhysicalVolume;
  G4PVPlacement * fPCBBottomPhysicalVolume;
  
  GigaTrackerSensorAssembly * fGigaTrackerSensorAssembly;
  GigaTrackerCoolingPlate * fGigaTrackerCoolingPlate;
};

#endif // GigaTrackerPCBModule
