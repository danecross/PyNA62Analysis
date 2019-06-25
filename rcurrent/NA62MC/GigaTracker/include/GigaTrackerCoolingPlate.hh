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
#ifndef GigaTrackerCoolingPlate_H
#define GigaTrackerCoolingPlate_H 1

#include "NA62VComponent.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "G4SubtractionSolid.hh"
#include "G4Trd.hh"
#include "G4Box.hh"
#include "G4PVReplica.hh"
#include "G4VSolid.hh"

class GigaTrackerCoolingPlate : public NA62VComponent
{
public: 
  ~GigaTrackerCoolingPlate();
  GigaTrackerCoolingPlate(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
private:
  G4int fiCopy;
  G4ThreeVector fPosition;
  
  G4double fChannelWallYLength;
  G4double fChannelWallZLength;
  G4double fChannelWallXLength;
  
  G4double fChannelsEnvelopeXLength;
  G4double fChannelsEnvelopeYLength; 
  G4double fChannelsEnvelopeZLength;
  
  G4double fTopPlateXLength;
  G4double fTopPlateYLength;
  G4double fTopPlateZLength;
  
  G4double fBottomPlateXLength;
  G4double fBottomPlateYLength;
  G4double fBottomPlateZLength;
  
  G4double fCoolingPlateXLength;
  G4double fCoolingPlateYLength;
  G4double fCoolingPlateZLength;
  
  G4double fChannelCoolantXLength;
  G4double fChannelCoolantYLength;
  G4double fChannelCoolantZLength;
  
  G4double fChannelsXOffset;
  G4double fChannelsDepth;
  
  G4double fChannelHalfWallXLength;
  G4double fChannelHalfWallYLength;
  G4double fChannelHalfWallZLength;
  
  G4double fChannelXLength;
  G4double fChannelYLength; 
  G4double fChannelZLength;
  
  G4double fTopShoulderXLength;
  G4double fTopHollowXLength;
  G4double fTopShoulderYLength;
  G4double fTopHollowYLength;
  G4double fTopDepth; 
  
  G4double fBottomShoulderXLength;
  G4double fBottomHollowXLength;
  G4double fBottomShoulderYLength;
  G4double fBottomHollowYLength;
  G4double fBottomDepth;
  
  G4Trd * fTopHollow;
  G4Trd * fBottomHollow;
  G4Box * fMainBloc;
  G4VSolid * fUpperWafer;
  
  
  G4Box * fChannelSolidVolume;
  G4LogicalVolume * fChannelLogicalVolume;
  G4VPhysicalVolume * fChannelPhysicalVolume;
  
  G4Box * fChannelHalfWallSolidVolume;
  G4LogicalVolume * fChannelHalfWallLogicalVolume;
  G4VPhysicalVolume * fChannelLeftWallPhysicalVolume;
  G4VPhysicalVolume * fChannelRightWallPhysicalVolume;
  
  G4Box * fChannelCoolantSolidVolume;
  G4LogicalVolume * fChannelCollantLogicalVolume;
  G4VPhysicalVolume *  fChannelCollantPhysicalVolume;
  
  G4Box * fTopPlateSolidVolume;
  G4LogicalVolume * fTopPlateLogicalVolume;
  G4VPhysicalVolume * fTopPlatePhysicalVolume;
  
  G4Box * fChannelsEnvelopeSolidVolume;
  G4LogicalVolume * fChannelsEnvelopeLogicalVolume;
  G4VPhysicalVolume * fChannelsEnvelopePhysicalVolume;
  
  G4Box * fChannelWallSolidVolume;
  G4LogicalVolume * fChannelWallLogicalVolume;
  G4VPhysicalVolume * fChannelWallPhysicalVolume;
  
  G4Box * fBottomPlateSolidVolume;
  G4LogicalVolume * fBottomPlateLogicalVolume;
  G4VPhysicalVolume * fBottomPlatePhysicalVolume;
};

#endif // GigaTrackerCoolingPlate
