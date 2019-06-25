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
// (Based on GigaTrackerStation.hh)
// --------------------------------------------------------------
//
#ifndef GigaTrackerBumpBonds_H
#define GigaTrackerBumpBonds_H 1

#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4PVPlacement.hh"
#include "G4PVParameterised.hh"

#include "NA62VComponent.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerBumpBondingParameterisation.hh"


class GigaTrackerBumpBonds : public NA62VComponent
{
public:
  GigaTrackerBumpBonds(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
private:
  G4int fiCopy;
  G4ThreeVector fPosition;
  
  G4double fBumpBondingRLength;
  G4double fBumpBondingZLength;
  G4double fSensorXLength;
  G4double fSensorYLength; 
  G4double fNumberOfPixels;
  
  GigaTrackerBumpBondingParameterisation * fBumpBondingParam; 
  G4Tubs * fBumpBondSolidVolume;
  G4LogicalVolume * fBumpBondLogicalVolume;
  G4VPhysicalVolume* fBumpBondPhysicalVolume;
};

#endif // GigaTrackerBumpBonds_H
