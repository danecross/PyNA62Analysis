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
#ifndef GigaTrackerSensor_H
#define GigaTrackerSensor_H 1

#include "NA62VComponent.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVParameterised.hh"
#include "G4PVPlacement.hh"
#include "GigaTrackerPixelParameterisation.hh"

class GigaTrackerSensor : public NA62VComponent
{

public:
  
  ~GigaTrackerSensor();
  GigaTrackerSensor(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
private:
  
  G4String fGigaTrackerSensitiveDetectorName;

  G4ThreeVector fPosition;
  G4int fiCopy;

  G4Box * fBorderASolidVolume;
  G4LogicalVolume * fBorderALogicalVolume;
  G4PVPlacement * fBorderTopPhysicalVolume;	
  G4PVPlacement * fBorderBottomPhysicalVolume;	
  
  G4Box * fBorderBSolidVolume;
  G4LogicalVolume * fBorderBLogicalVolume;
  G4PVPlacement * fBorderLeftPhysicalVolume;	
  G4PVPlacement * fBorderRightPhysicalVolume;	


  G4Box * fActiveAreaSolidVolume; 
  G4LogicalVolume * fActiveAreaLogicalVolume;
  G4PVPlacement * fActiveAreaPhysicalVolume;	

  //Pixel
  GigaTrackerPixelParameterisation * fPixelParameterisation;
  G4Box * fPixelSolidVolume;
  G4LogicalVolume * fPixelLogicalVolume;


  G4double fActiveSensorXLength;
  G4double fActiveSensorYLength;
  G4double fActiveSensorZLength;
  
  G4double fSensorXLength;
  G4double fSensorYLength;
  G4double fSensorZLength;

  G4int fNumberOfPixels;


  G4VisAttributes * fPixelVisAtt;
  G4VisAttributes * fBorderVisAtt;

};

#endif // GigaTrackerSensor
