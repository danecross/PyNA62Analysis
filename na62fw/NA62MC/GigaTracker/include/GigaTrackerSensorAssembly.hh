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
// (Based on GigaTrackerStation.hh)
// --------------------------------------------------------------
//
#ifndef GigaTrackerSensorAssembly_H
#define GigaTrackerSensorAssembly_H 1

#include <vector>

#include "NA62VComponent.hh"
#include "GigaTrackerGeometryParameters.hh"

#include "GigaTrackerSensor.hh"
#include "GigaTrackerChip.hh"
#include "GigaTrackerBumpBonds.hh"
#include "G4Box.hh"
#include "G4RotationMatrix.hh"

class GigaTrackerSensorAssembly : public NA62VComponent
{

public:
  
  ~GigaTrackerSensorAssembly();
  GigaTrackerSensorAssembly(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

private:
  G4int fiCopy;
  G4ThreeVector fPosition;

  G4double fGigaTrackerSensorAssemblyXLength;
  G4double fGigaTrackerSensorAssemblyYLength;
  G4double fGigaTrackerSensorAssemblyZLength;

  G4double fGigaTrackerActiveSensorXLength;

  G4double fGigaTrackerSensorZLength;
  G4double fGigaTrackerBumpBondingZLength;
  G4double fGigaTrackerGlueLayerZLength;
 
  G4double fGigaTrackerChipXLength;
  G4double fGigaTrackerChipYLength;
  G4double fGigaTrackerChipZLength;
  G4double fGigaTrackerChipXGap;


  G4Box * fGlueSolidVolume;
  G4LogicalVolume * fGlueLogicalVolume;
  G4VPhysicalVolume * fGluePhysicalVolume;

  GigaTrackerSensor * fGigaTrackerSensor;
  GigaTrackerBumpBonds * fGigaTrackerBumpBonds; 
  std::vector<GigaTrackerChip*> fGigaTrackerChips;

};

#endif // GigaTrackerSensorAssembly
