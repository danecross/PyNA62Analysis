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
// 2019-03 S.Schuchmann
// - Remove .db file and parameter class. Read parameter info from NA62Tools
// 2015-10-26 B. Velghe
// - Update the thickness of the cooling plate
//   (from 150 um to 370 um)
//
// 2015-05-12 B. Velghe
// - Update dimensions and position to the one used in 2014
//   + Readout chip thickness: From 100 um to 450 um (250 um) for station 1 and 3 (2). 
//   + Cooling plate thickness: From 130 um to 150 um.
//   + GTK Station 2 postion moved by 15 mm (Z axis) 20 mm (Y axis)
// 2014-03-01 B. Velghe
// - Add TRIM5 and BEND magnets mechanical structure
//
// 2012-03-27 B. Velghe
// - Merge the modifications made by G.Ruggiero
// - Magnet and collimator Z position updated (beatch file 2011-02-11)
// - Sign of the Trim 5 inverted
//
// 2009-03-12 S.Bifani
// - Added the collimator before station 3
// - Fixed the sign of the magnet fields
//
// 2009-03-03 A.Sergi
// - Added the UpdatePixelSize method to recompute pixel parameters
//
// 2009-03-03 S.Bifani
// - Removed the alluminum foil
// - Changed the geometrical configuration according to the 2008/11/24 BEATCH file
//   (x -> y deflection and 5th magnet)
//
/// 2008-05-06 S.Bifani
// - Fixed a dimension mismatch in X & Y lengths
// - Changed the magnet B fields according to the TRANSPORT file
//   (http://doble.web.cern.ch/doble/transport/k12hika+.txt 2008/02/29)
//
// 2008-04-29 S.Bifani
// - Implemeted the detailed pixel structure 
//   (sensitive silicon sensor, Sn-Pb bump bonding, silicon chip, carbon structure)
//
// 2008-04-22 S.Bifani (Simone.Bifani@cern.ch)
// - Added GTK parameters according to the BEATCH file 
//   (http://doble.web.cern.ch/doble/k12hika+.txt 2008/04/04)
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// --------------------------------------------------------------
#include "GigaTrackerGeometryParameters.hh"
#include "DetectorParameter.hh"
#include "TVector.h"
#include "GigaTrackerParameterTools.hh"
#include "NA62ConditionsService.hh"

/////////////////////////
// Geometry Parameters //
/////////////////////////

GigaTrackerGeometryParameters* GigaTrackerGeometryParameters::fInstance = nullptr;

GigaTrackerGeometryParameters::GigaTrackerGeometryParameters() : NA62VGeometryParameters(G4String("GigaTracker")) {

  int runID = NA62ConditionsService::GetInstance()->GetCurrentRunID();
  bool isMC = true;
  fParameters = GigaTrackerParameterTools::GetInstance(runID, isMC);

  fnStation = fParameters->GetNStations();

  // Define all the geometrical parameters and build the
  // responsibility region accordingly
  
  // Sensitive detector
  fGigaTrackerSensitiveDetectorName = (fParameters->GetGigaTrackerSensitiveDetectorName());
  fGigaTrackerCollectionName        = (fParameters->GetGigaTrackerCollectionName());
  
  
  // Unused World Parameters for stand-alone configurations
  fWorldXLength = double((fParameters->GetWorldLength()).X())*mm;
  fWorldYLength = double((fParameters->GetWorldLength()).Y())*mm;
  fWorldZLength = double((fParameters->GetWorldLength()).Z())*mm;
  
  // Central detector position (G4 frame)
  fGigaTrackerDetectorZPosition = double((fParameters->GetGigaTrackerDetectorZPosition()))*mm;

  // Responsability region
  fGigaTrackerDetectorXLength = double((fParameters->GetGigaTrackerResponseRegion()).X())*mm;
  fGigaTrackerDetectorYLength = double((fParameters->GetGigaTrackerResponseRegion()).Y())*mm;
  fGigaTrackerDetectorZLength = double((fParameters->GetGigaTrackerResponseRegion()).Z())*mm;
 
  fResponsibilityRegion.push_back
    (new ResponsibilityRegion (fGigaTrackerDetectorZPosition - 0.5*fGigaTrackerDetectorZLength,
                               fGigaTrackerDetectorZPosition + 0.5*fGigaTrackerDetectorZLength));
  
  
  for(int i = 0; i < fnStation; i++) {
    //stations included
    fStationIn[i] = bool(fParameters->GetStationIn(i));

    // Station dimension
    fGigaTrackerStationXLength[i] = double((fParameters->GetGigaTrackerStationLength(i)).X())*mm;
    fGigaTrackerStationYLength[i] = double((fParameters->GetGigaTrackerStationLength(i)).Y())*mm;
    fGigaTrackerStationZLength[i] = double((fParameters->GetGigaTrackerStationLength(i)).Z())*mm;


    // Sensor effective size 
    fGigaTrackerSensorXLength[i] = double((fParameters->GetGigaTrackerSensorLength(i)).X())*mm;
    fGigaTrackerSensorYLength[i] = double((fParameters->GetGigaTrackerSensorLength(i)).Y())*mm;
    fGigaTrackerSensorZLength[i] = double((fParameters->GetGigaTrackerSensorLength(i)).Z())*mm;

    // Sensor active area
    fGigaTrackerActiveSensorXLength[i] = double((fParameters->GetGigaTrackerActiveSensorLength(i)).X())*mm;
    fGigaTrackerActiveSensorYLength[i] = double((fParameters->GetGigaTrackerActiveSensorLength(i)).Y())*mm;
    fGigaTrackerActiveSensorZLength[i] = double((fParameters->GetGigaTrackerActiveSensorLength(i)).Z())*mm;

    // Sensor assembly dimension 
    fGigaTrackerSensorAssemblyXLength[i] = double((fParameters->GetGigaTrackerSensorAssemblyLength(i)).X())*mm;
    fGigaTrackerSensorAssemblyYLength[i] = double((fParameters->GetGigaTrackerSensorAssemblyLength(i)).Y())*mm;
    fGigaTrackerSensorAssemblyZLength[i] = double((fParameters->GetGigaTrackerSensorAssemblyLength(i)).Z())*mm;
    

    // Chip dimension
    fGigaTrackerChipXLength[i] = double((fParameters->GetGigaTrackerChipLength(i)).X())*mm;
    fGigaTrackerChipYLength[i] = double((fParameters->GetGigaTrackerChipLength(i)).Y())*mm;
    fGigaTrackerChipZLength[i] = double((fParameters->GetGigaTrackerChipLength(i)).Z())*mm;


    // Cooling plate dimension
    fCoolingPlateXLength[i] = double((fParameters->GetCoolingPlateLength(i)).X())*mm;
    fCoolingPlateYLength[i] = double((fParameters->GetCoolingPlateLength(i)).Y())*mm;
    fCoolingPlateZLength[i] = double((fParameters->GetCoolingPlateLength(i)).Z())*mm;
  }

  // Bump Bonding dimension
  fGigaTrackerBumpBondingRLength = double(fParameters->GetGigaTrackerBumpBondingRLength())*mm;
  fGigaTrackerBumpBondingZLength = double(fParameters->GetGigaTrackerBumpBondingZLength())*mm;
  // Bump bonds offsets (relative to pixel center)
  fGigaTrackerBumpBondOffset     = double(fParameters->GetGigaTrackerBumpBondPixelOffset())*mm;
  fGigaTrackerBumpBondBigOffset  = double(fParameters->GetGigaTrackerBumpBondBigOffset())*mm;
  fGigaTrackerBumpBondingXOffset = double((fParameters->GetGigaTrackerBumpBondingOffset()).X())*mm;
  fGigaTrackerBumpBondingYOffset = double((fParameters->GetGigaTrackerBumpBondingOffset()).Y())*mm;
  
  // Chip gap
  fGigaTrackerChipXGap = double((fParameters->GetGigaTrackerChipGap()))*mm;

  // Glue layer
  fGigaTrackerGlueLayerZLength = double((fParameters->GetGigaTrackerGlueLayerZLength()))*mm;

  // Pixel matrix configuration
  fGigaTrackerNumberOfPixels = int(fParameters->GetGigaTrackerNumberOfPixels());
  fGigaTrackerSensorNColumns = int(fParameters->GetGigaTrackerSensorNColumns());
  fGigaTrackerSensorNRows    = int(fParameters->GetGigaTrackerSensorNRows());
  fGigaTrackerChipNColumns   = int(fParameters->GetGigaTrackerChipNColumns());
  fGigaTrackerChipNRows      = int(fParameters->GetGigaTrackerChipNRows());

  // Pixel dimensions
  fGigaTrackerSmallPixelXLength = double((fParameters->GetGigaTrackerSmallPixelLength()).X())*mm;
  fGigaTrackerSmallPixelYLength = double((fParameters->GetGigaTrackerSmallPixelLength()).Y())*mm;
  fGigaTrackerBigPixelXLength   = double((fParameters->GetGigaTrackerBigPixelLength()).X())*mm;
  fGigaTrackerBigPixelYLength   = double((fParameters->GetGigaTrackerBigPixelLength()).Y())*mm;
  fGigaTrackerPixelZLength      = double((fParameters->GetGigaTrackerBigPixelLength()).Z())*mm;


  /////////////////
  //Cooling Plates//
  /////////////////
  //Top
  fCoolingPlateTopShoulderXLength = double((fParameters->GetCoolingPlateTopShoulderLength()).X())*mm;
  fCoolingPlateTopShoulderYLength = double((fParameters->GetCoolingPlateTopShoulderLength()).Y())*mm;
  fCoolingPlateTopHollowXLength   = double((fParameters->GetCoolingPlateTopHollowLength()).X())*mm;
  fCoolingPlateTopHollowYLength   = double((fParameters->GetCoolingPlateTopHollowLength()).Y())*mm;
  for(int i =0 ;i <fnStation; i++)  fCoolingPlateTopDepth[i] = double((fParameters->GetCoolingPlateTopDepth(i)))*mm;
  
  //Bottom
  fCoolingPlateBottomShoulderXLength = double((fParameters->GetCoolingPlateBottomShoulderLength()).X())*mm;
  fCoolingPlateBottomShoulderYLength = double((fParameters->GetCoolingPlateBottomShoulderLength()).Y())*mm;
  fCoolingPlateBottomHollowXLength   = double((fParameters->GetCoolingPlateBottomHollowLength()).X())*mm;
  fCoolingPlateBottomHollowYLength   = double((fParameters->GetCoolingPlateBottomHollowLength()).Y())*mm;
  for(int i =0 ;i <fnStation; i++) fCoolingPlateBottomDepth[i] = double((fParameters->GetCoolingPlateBottomDepth(i)))*mm;
  
  //Channels
  fCoolingPlateChannelsEnvelopeXLength = double((fParameters->GetCoolingPlateChannelsEnvelopeLength()).X())*mm;
  fCoolingPlateChannelsEnvelopeYLength = double((fParameters->GetCoolingPlateChannelsEnvelopeLength()).Y())*mm;
  fCoolingPlateChannelsEnvelopeZLength = double((fParameters->GetCoolingPlateChannelsEnvelopeLength()).Z())*mm;
  fCoolingPlateChannelsXOffset         = double((fParameters->GetCoolingPlateChannelsXOffset()))*mm;
  fCoolingPlateChannelsDepth           = double((fParameters->GetCoolingPlateChannelsDepth()))*mm;
  fCoolingPlateChannelCoolantXLength   = double((fParameters->GetCoolingPlateChannelCoolantXLength()))*mm;
  fCoolingPlateChannelHalfWallXLength  = double((fParameters->GetCoolingPlateChannelHalfWallXLength()))*mm;

  // PCB
  fGigaTrackerPCBXLength     = double((fParameters->GetGigaTrackerPCBLength()).X())*mm;
  fGigaTrackerPCBYLength     = double((fParameters->GetGigaTrackerPCBLength()).Y())*mm;
  fGigaTrackerPCBZLength     = double((fParameters->GetGigaTrackerPCBLength()).Z())*mm;
  fGigaTrackerPCBHoleXLength = double((fParameters->GetGigaTrackerPCBHoleLength()).X())*mm;
  fGigaTrackerPCBHoleYLength = double((fParameters->GetGigaTrackerPCBHoleLength()).Y())*mm;
  fGigaTrackerPCBHoleXOffset = double((fParameters->GetGigaTrackerPCBHoleXOffset()))*mm;

  // --------------------
  // |                  |
  // |         |-----|  |
  // |<------->|  0  |  |
  // |         |-----|  |
  // |                  |
  // --------------------


  ////////////////
  // MCB Magnet //
  ////////////////
  // Magnet dimensions
  fGigaTrackerMCBMagnetXLength = double((fParameters->GetGigaTrackerMCBMagnetLength()).X())*mm;
  fGigaTrackerMCBMagnetYLength = double((fParameters->GetGigaTrackerMCBMagnetLength()).Y())*mm;
  fGigaTrackerMCBMagnetZLength = double((fParameters->GetGigaTrackerMCBMagnetLength()).Z())*mm;
  fGigaTrackerMDXMagnetXLength = double((fParameters->GetGigaTrackerMDXMagnetLength()).X())*mm;
  fGigaTrackerMDXMagnetYLength = double((fParameters->GetGigaTrackerMDXMagnetLength()).Y())*mm;
  fGigaTrackerMDXMagnetZLength = double((fParameters->GetGigaTrackerMDXMagnetLength()).Z())*mm;

  fGigaTrackerMCBMagnetFieldXLength = double((fParameters->GetGigaTrackerMCBMagnetFieldLength()).X())*mm;
  fGigaTrackerMCBMagnetFieldYLength = double((fParameters->GetGigaTrackerMCBMagnetFieldLength()).Y())*mm;
  fGigaTrackerMCBMagnetFieldZLength = double((fParameters->GetGigaTrackerMCBMagnetFieldLength()).Z())*mm;
  fGigaTrackerMCBMagnetBaseYLength  = double((fParameters->GetGigaTrackerMCBMagnetBaseYLength()))*mm;
  fGigaTrackerMCBMagnetGapXLength   = double((fParameters->GetGigaTrackerMCBMagnetGapXLength()))*mm;
  fGigaTrackerMCBMagnetSideYLength  = double((fParameters->GetGigaTrackerMCBMagnetSideYLength()))*mm;
  fGigaTrackerMCBMagnetHatYLength   = double((fParameters->GetGigaTrackerMCBMagnetHatYLength()))*mm;
  //Beam pos (from the *bottom* of the base)
  fGigaTrackerMCBMagnetBeamYPos     = double((fParameters->GetGigaTrackerMCBMagnetBeamYPos()))*mm;

  fGigaTrackerArchomatMagnetFieldStrength[0] = double(fParameters->GetGigaTrackerMCBMagnetFieldStrength(0))*tesla;
  fGigaTrackerArchomatMagnetFieldStrength[1] = double(fParameters->GetGigaTrackerMCBMagnetFieldStrength(1))*tesla;
  fGigaTrackerArchomatMagnetFieldStrength[2] = double(fParameters->GetGigaTrackerMCBMagnetFieldStrength(2))*tesla;
  fGigaTrackerArchomatMagnetFieldStrength[3] = double(fParameters->GetGigaTrackerMCBMagnetFieldStrength(3))*tesla;
						      

  ////////////////
  // MDX Magnet //
  ////////////////
  fGigaTrackerMDXMagnetFieldXLength = double((fParameters->GetGigaTrackerMDXMagnetFieldLength()).X())*mm;
  fGigaTrackerMDXMagnetFieldYLength = double((fParameters->GetGigaTrackerMDXMagnetFieldLength()).Y())*mm;
  fGigaTrackerMDXMagnetFieldZLength = double((fParameters->GetGigaTrackerMDXMagnetFieldLength()).Z())*mm;
  fGigaTrackerMDXMagnetGapXLength   = double((fParameters->GetGigaTrackerMDXMagnetGapLength()).X())*mm;
  fGigaTrackerMDXMagnetGapYLength   = double((fParameters->GetGigaTrackerMDXMagnetGapLength()).Y())*mm;

  fGigaTrackerTRIM5MagnetFieldStrength = double(fParameters->GetGigaTrackerMDXMagnetFieldStrength())*tesla;


  ////////////////////////////
  // The beam scraper magnet//
  ////////////////////////////
  fGigaTrackerScraperMagnetZLength            = double((fParameters->GetGigaTrackerScraperMagnetLength().Z()))*mm;
  fGigaTrackerScraperMagnetApertureHalfWidth  = double((fParameters->GetGigaTrackerScraperMagnetApertureHalfWidth()))*mm;
  fGigaTrackerScraperMagnetApertureHalfHeight = double((fParameters->GetGigaTrackerScraperMagnetApertureHalfHeight()))*mm;
  fGigaTrackerScraperMagnetOverallHalfHeight  = double((fParameters->GetGigaTrackerScraperMagnetOverallHalfHeight()))*mm;
  fGigaTrackerScraperMagnetFieldStrength      = double(fParameters->GetGigaTrackerScraperMagnetFieldStrength())*tesla;


  /////////////////////////
  // Collimator dimension//
  /////////////////////////
  fGigaTrackerCollimatorOuterXLength = double((fParameters->GetGigaTrackerCollimatorOuterLength()).X())*mm;
  fGigaTrackerCollimatorOuterYLength = double((fParameters->GetGigaTrackerCollimatorOuterLength()).Y())*mm;
  fGigaTrackerCollimatorInnerXLength = double((fParameters->GetGigaTrackerCollimatorInnerLength()).X())*mm;
  fGigaTrackerCollimatorInnerYLength = double((fParameters->GetGigaTrackerCollimatorInnerLength()).Y())*mm;
  fGigaTrackerCollimatorZLength      = double((fParameters->GetGigaTrackerCollimatorInnerLength()).Z())*mm;
  fGigaTrackerCollimatorDesign       = fParameters->GetGigaTrackerCollimatorDesign();
  if(fGigaTrackerCollimatorDesign.contains("2018")) 
    fGigaTrackerCollimatorGDML      = (fParameters->GetGigaTrackerCollimatorGDML());

  ///////////////
  // Positions //
  ///////////////

  for(int i =0 ;i <fnStation; i++){
        
    // Station
    fGigaTrackerStationPosition[i] =  G4ThreeVector(double((fParameters->GetGigaTrackerStationPositionMC(i)).X())*mm,
						    double((fParameters->GetGigaTrackerStationPositionMC(i)).Y())*mm, 
						    double((fParameters->GetGigaTrackerStationPositionMC(i)).Z())*mm);    
      
      // Bump bonding
    fGigaTrackerBumpBondingPosition[i] = G4ThreeVector(double((fParameters->GetGigaTrackerBumpBondingPosition(i)).X())*mm,
						       double((fParameters->GetGigaTrackerBumpBondingPosition(i)).Y())*mm,
						       double((fParameters->GetGigaTrackerBumpBondingPosition(i)).Z())*mm);
    // Depreciated
    // Structure position
    //for(int i = 0; i < 3; i++) {
    //  fGigaTrackerSupportPosition[i] = G4ThreeVector(double((fParameters->GetGigaTrackerSupportPosition(i)).X())*mm,
    //  double((fParameters->GetGigaTrackerSupportPosition(i)).Y())*mm,
    //  double((fParameters->GetGigaTrackerSupportPosition(i)).Z())*mm);
    //}

    // Sensor
    fGigaTrackerSensorPosition[i] = G4ThreeVector(double((fParameters->GetGigaTrackerSensorPosition(i)).X())*mm,
						  double((fParameters->GetGigaTrackerSensorPosition(i)).Y())*mm,
						  double((fParameters->GetGigaTrackerSensorPosition(i)).Z())*mm);
    
    // Chip
    fGigaTrackerChipPosition[i]   = G4ThreeVector(double((fParameters->GetGigaTrackerChipPosition(i)).X())*mm,
						  double((fParameters->GetGigaTrackerChipPosition(i)).Y())*mm,
						  double((fParameters->GetGigaTrackerChipPosition(i)).Z())*mm);
  }
  

  //////////////////////////////////////////////////////////////////////////////////////
  // Magnet positions: the z coordinates are those of the downstream ends of the magnets
  // BEND 4A: 80.480-82.980 m
  fGigaTrackerMagnet1Position = G4ThreeVector(double((fParameters->GetGigaTrackerMCBMagnetPosition(0)).X())*mm,
					      double((fParameters->GetGigaTrackerMCBMagnetPosition(0)).Y())*mm,
					      double((fParameters->GetGigaTrackerMCBMagnetPosition(0)).Z())*mm);
  // BEND 4B: 84.080-86.580 m
  fGigaTrackerMagnet2Position = G4ThreeVector(double((fParameters->GetGigaTrackerMCBMagnetPosition(1)).X())*mm,
					      double((fParameters->GetGigaTrackerMCBMagnetPosition(1)).Y())*mm,
					      double((fParameters->GetGigaTrackerMCBMagnetPosition(1)).Z())*mm);
  // BEND 5:  93.360-95.860 m
  fGigaTrackerMagnet3Position = G4ThreeVector(double((fParameters->GetGigaTrackerMCBMagnetPosition(2)).X())*mm,
					      double((fParameters->GetGigaTrackerMCBMagnetPosition(2)).Y())*mm,
					      double((fParameters->GetGigaTrackerMCBMagnetPosition(2)).Z())*mm);
  // BEND 6:  96.960-99.460 m
  fGigaTrackerMagnet4Position = G4ThreeVector(double((fParameters->GetGigaTrackerMCBMagnetPosition(3)).X())*mm,
					      double((fParameters->GetGigaTrackerMCBMagnetPosition(3)).Y())*mm,
					      double((fParameters->GetGigaTrackerMCBMagnetPosition(3)).Z())*mm);
  // TRIM 5:  101.600-102.000 m
  fGigaTrackerMagnet5Position = G4ThreeVector(double((fParameters->GetGigaTrackerMDXMagnetPosition()).X())*mm,
					      double((fParameters->GetGigaTrackerMDXMagnetPosition()).Y())*mm,
					      double((fParameters->GetGigaTrackerMDXMagnetPosition()).Z())*mm);
  // Scraper
  fGigaTrackerScraperMagnetPosition = G4ThreeVector(double((fParameters->GetGigaTrackerScraperMagnetPosition()).X())*mm,
						    double((fParameters->GetGigaTrackerScraperMagnetPosition()).Y())*mm,
						    double((fParameters->GetGigaTrackerScraperMagnetPosition()).Z())*mm);
  // Collimator position
  fGigaTrackerCollimatorPosition    = G4ThreeVector(double((fParameters->GetGigaTrackerCollimatorPosition()).X())*mm,
						    double((fParameters->GetGigaTrackerCollimatorPosition()).Y())*mm,
						    double((fParameters->GetGigaTrackerCollimatorPosition()).Z())*mm);
}

GigaTrackerGeometryParameters* GigaTrackerGeometryParameters::GetInstance() {
  if (fInstance == 0) fInstance = new GigaTrackerGeometryParameters();
  return fInstance;
}

//
// Warning: This method is not up-to-date. 
//
TObjArray GigaTrackerGeometryParameters::GetHashTable() {

  TObjArray GigaTrackerGeometryParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fWorldZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldZLength));
  GigaTrackerGeometryParameters.Add(new DetectorParameter("fWorldZLength", Value.Data(),
							  "World Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldXLength));
  GigaTrackerGeometryParameters.Add(new DetectorParameter("fWorldXLength", Value.Data(),
							  "World X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldYLength));
  GigaTrackerGeometryParameters.Add(new DetectorParameter("fWorldYLength", Value.Data(),
							  "World Y Length", ParameterData));
  ParameterData.Clear();

  Buffer << fGigaTrackerDetectorZPosition;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fGigaTrackerDetectorZPosition));
  GigaTrackerGeometryParameters.Add(new DetectorParameter("fGigaTrackerDetectorZPosition", Value.Data(),
							  "GigaTracker Detector Z Position", ParameterData));
  ParameterData.Clear();

  Buffer << fGigaTrackerDetectorZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fGigaTrackerDetectorZLength));
  GigaTrackerGeometryParameters.Add(new DetectorParameter("fGigaTrackerDetectorZLength", Value.Data(),
							  "GigaTracker Detector Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fGigaTrackerDetectorXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fGigaTrackerDetectorXLength));
  GigaTrackerGeometryParameters.Add(new DetectorParameter("fGigaTrackerDetectorXLength", Value.Data(),
							  "GigaTracker Detector X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fGigaTrackerDetectorYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fGigaTrackerDetectorYLength));
  GigaTrackerGeometryParameters.Add(new DetectorParameter("fGigaTrackerDetectorYLength", Value.Data(),
							  "GigaTracker Detector Y Length", ParameterData));

  // Add for Pixel

  return GigaTrackerGeometryParameters;

}


//
// Warning: This method is not up-to-date. 
//
void GigaTrackerGeometryParameters::Print()
{

  G4cout << "fGigaTrackerDetectorZPosition= " << fGigaTrackerDetectorZPosition << G4endl

	 << "fGigaTrackerDetectorXLength= " << fGigaTrackerDetectorXLength << G4endl
	 << "fGigaTrackerDetectorYLength= " << fGigaTrackerDetectorYLength << G4endl
	 << "fGigaTrackerDetectorZLength= " << fGigaTrackerDetectorZLength << G4endl

	 << fWorldXLength << G4endl
	 << fWorldYLength << G4endl
	 << fWorldZLength << G4endl


	 << "StationPos \n"<<fGigaTrackerStationPosition[0].getX() << " " << fGigaTrackerStationPosition[0].getY() << " " << fGigaTrackerStationPosition[0].getZ() << G4endl
	 << fGigaTrackerStationPosition[1].getX() << " " << fGigaTrackerStationPosition[1].getY() << " " << fGigaTrackerStationPosition[1].getZ() << G4endl
	 << fGigaTrackerStationPosition[2].getX() << " " << fGigaTrackerStationPosition[2].getY() << " " << fGigaTrackerStationPosition[2].getZ() << G4endl
	 << fGigaTrackerStationXLength[0] << G4endl
	 << fGigaTrackerStationYLength[0] << G4endl
	 << fGigaTrackerStationZLength[0] << G4endl

	 << "Sensors \n"<<fGigaTrackerSensorPosition[0].getX() << " " << fGigaTrackerSensorPosition[0].getY() << " " << fGigaTrackerSensorPosition[0].getZ() << G4endl
	 << fGigaTrackerSensorNColumns << G4endl
	 << fGigaTrackerSensorNRows << G4endl

	 << fGigaTrackerSensorXLength[0] << G4endl
	 << fGigaTrackerSensorYLength[0] << G4endl
	 << fGigaTrackerSensorZLength[0] << G4endl

	 << fGigaTrackerSensorAssemblyXLength[0] << G4endl
	 << fGigaTrackerSensorAssemblyYLength[0] << G4endl
	 << fGigaTrackerSensorAssemblyZLength[0] << G4endl

	 << fGigaTrackerActiveSensorXLength[0] << G4endl
	 << fGigaTrackerActiveSensorYLength[0] << G4endl
	 << fGigaTrackerActiveSensorZLength[0] << G4endl

	 << fGigaTrackerChipPosition[0].getX() << " " << fGigaTrackerChipPosition[0].getY() << " " << fGigaTrackerChipPosition[0].getZ() << G4endl
	 << fGigaTrackerChipXGap << G4endl
	 << fGigaTrackerGlueLayerZLength << G4endl

	 << fGigaTrackerChipXLength[0] << G4endl
	 << fGigaTrackerChipYLength[0] << G4endl
	 << fGigaTrackerChipZLength[0] << G4endl

	 << fGigaTrackerChipNColumns << G4endl
	 << fGigaTrackerChipNRows << G4endl

	 << fGigaTrackerBigPixelXLength << G4endl
	 << fGigaTrackerBigPixelYLength << G4endl
	 << fGigaTrackerSmallPixelXLength << G4endl
	 << fGigaTrackerSmallPixelYLength << G4endl
	 << fGigaTrackerPixelZLength << G4endl
	 << fGigaTrackerNumberOfPixels << G4endl

	 << "Cooling \n"<<fCoolingPlateXLength[0] << G4endl
	 << fCoolingPlateYLength[0] << G4endl
	 << fCoolingPlateZLength[0] << G4endl

	 << fCoolingPlateTopShoulderXLength << G4endl
	 << fCoolingPlateTopHollowXLength << G4endl
	 << fCoolingPlateTopShoulderYLength << G4endl
	 << fCoolingPlateTopHollowYLength << G4endl
	 << fCoolingPlateTopDepth[0] << G4endl
	 << fCoolingPlateTopDepth[1] << G4endl
	 << fCoolingPlateTopDepth[2] << G4endl
    
	 << fCoolingPlateBottomShoulderXLength << G4endl
	 << fCoolingPlateBottomHollowXLength << G4endl
	 << fCoolingPlateBottomShoulderYLength << G4endl
	 << fCoolingPlateBottomHollowYLength << G4endl
	 << fCoolingPlateBottomDepth[0] << G4endl
	 << fCoolingPlateBottomDepth[1] << G4endl
	 << fCoolingPlateBottomDepth[2] << G4endl
    
	 << fCoolingPlateChannelsEnvelopeXLength << G4endl
	 << fCoolingPlateChannelsEnvelopeYLength << G4endl
	 << fCoolingPlateChannelsEnvelopeZLength << G4endl
	 << fCoolingPlateChannelsXOffset << G4endl
	 << fCoolingPlateChannelsDepth << G4endl
	 << fCoolingPlateChannelCoolantXLength << G4endl
	 << fCoolingPlateChannelHalfWallXLength << G4endl
    
	 << fGigaTrackerBumpBondingRLength << G4endl
	 << fGigaTrackerBumpBondingZLength << G4endl
	 << fGigaTrackerBumpBondOffset << G4endl
	 << fGigaTrackerBumpBondBigOffset << G4endl
	 << fGigaTrackerBumpBondingXOffset << G4endl
	 << fGigaTrackerBumpBondingYOffset << G4endl
	 << fGigaTrackerBumpBondingPosition[0].getX() << " " << fGigaTrackerBumpBondingPosition[0].getY() << " " << fGigaTrackerBumpBondingPosition[0].getZ() << G4endl 
    
	 << fGigaTrackerPCBXLength << G4endl
	 << fGigaTrackerPCBYLength << G4endl
	 << fGigaTrackerPCBZLength << G4endl
	 << fGigaTrackerPCBHoleXLength << G4endl
	 << fGigaTrackerPCBHoleYLength << G4endl
	 << fGigaTrackerPCBHoleXOffset << G4endl
    
	 << "Magnets\n"<<fGigaTrackerMCBMagnetXLength << G4endl
	 << fGigaTrackerMCBMagnetYLength << G4endl
	 << fGigaTrackerMCBMagnetZLength << G4endl
	 << fGigaTrackerMCBMagnetBaseYLength << G4endl
	 << fGigaTrackerMCBMagnetGapXLength << G4endl
	 << fGigaTrackerMCBMagnetSideYLength << G4endl
	 << fGigaTrackerMCBMagnetHatYLength << G4endl
	 << fGigaTrackerMCBMagnetBeamYPos << G4endl
	 << fGigaTrackerMCBMagnetFieldXLength << G4endl
	 << fGigaTrackerMCBMagnetFieldYLength << G4endl
	 << fGigaTrackerMCBMagnetFieldZLength << G4endl

	 << fGigaTrackerMDXMagnetXLength << G4endl
	 << fGigaTrackerMDXMagnetYLength << G4endl
	 << fGigaTrackerMDXMagnetZLength << G4endl
	 << fGigaTrackerMDXMagnetFieldXLength << G4endl
	 << fGigaTrackerMDXMagnetFieldYLength << G4endl
	 << fGigaTrackerMDXMagnetFieldZLength << G4endl
	 << fGigaTrackerMDXMagnetGapXLength << G4endl
	 << fGigaTrackerMDXMagnetGapYLength << G4endl

	 << fGigaTrackerScraperMagnetZLength << G4endl
	 << fGigaTrackerScraperMagnetApertureHalfWidth << G4endl
	 << fGigaTrackerScraperMagnetApertureHalfHeight << G4endl
	 << fGigaTrackerScraperMagnetOverallHalfHeight << G4endl
	 << fGigaTrackerScraperMagnetFieldStrength << G4endl
    
	 << fGigaTrackerCollimatorOuterXLength << G4endl
	 << fGigaTrackerCollimatorOuterYLength << G4endl
	 << fGigaTrackerCollimatorInnerXLength << G4endl
	 << fGigaTrackerCollimatorInnerYLength << G4endl
	 << fGigaTrackerCollimatorZLength << G4endl

	 << fGigaTrackerArchomatMagnetFieldStrength[0] << G4endl
	 << fGigaTrackerArchomatMagnetFieldStrength[1] << G4endl
	 << fGigaTrackerArchomatMagnetFieldStrength[2] << G4endl
	 << fGigaTrackerArchomatMagnetFieldStrength[3] << G4endl
    
	 << fGigaTrackerTRIM5MagnetFieldStrength << G4endl

	 << fGigaTrackerMagnet1Position.getX() << " " << fGigaTrackerMagnet1Position.getY() << " " << fGigaTrackerMagnet1Position.getZ() << G4endl
	 << fGigaTrackerMagnet2Position.getX() << " " << fGigaTrackerMagnet2Position.getY() << " " << fGigaTrackerMagnet2Position.getZ() << G4endl
	 << fGigaTrackerMagnet3Position.getX() << " " << fGigaTrackerMagnet3Position.getY() << " " << fGigaTrackerMagnet3Position.getZ() << G4endl
	 << fGigaTrackerMagnet4Position.getX() << " " << fGigaTrackerMagnet4Position.getY() << " " << fGigaTrackerMagnet4Position.getZ() << G4endl
	 << fGigaTrackerMagnet5Position.getX() << " " << fGigaTrackerMagnet5Position.getY() << " " << fGigaTrackerMagnet5Position.getZ() << G4endl

	 << fGigaTrackerCollimatorPosition.getX() << " " << fGigaTrackerCollimatorPosition.getY() << " " << fGigaTrackerCollimatorPosition.getZ() << G4endl;


}

//
// Return true if pixel <id> is big, else return false
//
G4bool GigaTrackerGeometryParameters::PixelIsBig(G4int id) const {
  G4int N = (id/fGigaTrackerSensorNColumns)*fGigaTrackerSensorNColumns;
  G4int Xindex = (id - N);
  // 40 / 80 / 120 / 160
  if(Xindex%fGigaTrackerChipNColumns == 0 && Xindex != 0) return true;
  // 39 / 79 / 119 / 159
  if((Xindex + 1)%fGigaTrackerChipNColumns == 0 && Xindex != (fGigaTrackerSensorNColumns - 1)) return true;
  return false;
}

//
// Return the distance between the sensor edge and the pixel *edge* 
// 
//
//      <N>
//       v
//  sensorRows-1 [0|1|2|...|sensorColumns -1]
//      ...      [...                    ...]
//       0       [0|1|2|...|sensorColumns -1]
//                ^ 
//             <Xindex>
//
G4double GigaTrackerGeometryParameters::PixelXOffset(G4int id) const {
  G4int N = (id/fGigaTrackerSensorNColumns)*fGigaTrackerSensorNColumns;
  G4int Xindex = (id - N);
  // N_big_*: # big pixels *before* pixel <id> 
  G4int N_big_1 = (Xindex - 1)/fGigaTrackerChipNColumns; // 41/81/121/161
  G4int N_big_2 = Xindex/fGigaTrackerChipNColumns;       // 40/80/120/160 
  return (Xindex - N_big_1 - N_big_2)*fGigaTrackerSmallPixelXLength + (N_big_1 + N_big_2)*fGigaTrackerBigPixelXLength; 
}

//////////////////////////////
// Pixels position and size //
//////////////////////////////

// ! Positions are relative to the corner of the sensor !

G4double GigaTrackerGeometryParameters::GetPixelXPosition(G4int id) const {
  if (PixelIsBig(id)) return PixelXOffset(id) + 0.5*fGigaTrackerBigPixelXLength; 
  else return PixelXOffset(id) + 0.5*fGigaTrackerSmallPixelXLength;

}

G4double GigaTrackerGeometryParameters::GetPixelYPosition(G4int id) const {
  return 0.5*fGigaTrackerSmallPixelYLength + (id/fGigaTrackerSensorNColumns)*fGigaTrackerSmallPixelYLength;
}

G4double GigaTrackerGeometryParameters::GetPixelXLength(G4int id) const {
  if (PixelIsBig(id)) return fGigaTrackerBigPixelXLength;
  return fGigaTrackerSmallPixelXLength;
}

G4double GigaTrackerGeometryParameters::GetPixelYLength(G4int) const {
  return fGigaTrackerSmallPixelYLength;
}

/////////////////////////
// Bump bonds position //
/////////////////////////
G4double GigaTrackerGeometryParameters::GetBumpBondXPosition(G4int id) const { 
  G4double offset = 0.0;  
  if (PixelIsBig(id)) offset = fGigaTrackerBumpBondBigOffset;
  if (id%2) return GetPixelXPosition(id) + (fGigaTrackerBumpBondOffset + offset); 
  else return GetPixelXPosition(id) - (fGigaTrackerBumpBondOffset + offset);  
} 

G4double GigaTrackerGeometryParameters::GetBumpBondYPosition(G4int id) const {
  if ( (id/fGigaTrackerSensorNColumns) > (fGigaTrackerChipNRows - 1) ) return GetPixelYPosition(id) + fGigaTrackerBumpBondOffset;
  else return GetPixelYPosition(id) - fGigaTrackerBumpBondOffset;
}
