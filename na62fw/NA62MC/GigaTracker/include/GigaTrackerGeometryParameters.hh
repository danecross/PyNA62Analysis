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
// 2014-03-01 B.Velghe (bob.velghe@cern.ch)
//  - Add TIRM5 and BEND magnets mechanical structure
//
// 2012-03-09 B.Velghe (bob.velghe@cern.ch)
//
// 2008-04-22 S.Bifani (Simone.Bifani@cern.ch)
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// --------------------------------------------------------------
//
#ifndef GigaTrackerGeometryParameters_H
#define GigaTrackerGeometryParameters_H 1

#include "globals.hh"
#include "NA62VGeometryParameters.hh"
#include "TObjArray.h"
#include "G4ThreeVector.hh"
#include "GigaTrackerParameterTools.hh"

class GigaTrackerGeometryParameters : public NA62VGeometryParameters {
  static const int fnStationMax = 4;
public:

  ~GigaTrackerGeometryParameters() {}
  static GigaTrackerGeometryParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static GigaTrackerGeometryParameters* fInstance;
 
  // Helpers 
  G4bool PixelIsBig(G4int id) const;
  G4double PixelXOffset(G4int id) const;
protected:

  GigaTrackerGeometryParameters();

public:
  G4double GetPixelXPosition(G4int id) const;
  G4double GetPixelYPosition(G4int id) const;
  G4double GetPixelXLength(G4int id) const;
  G4double GetPixelYLength(G4int id) const;
  G4double GetBumpBondXPosition(G4int id) const;
  G4double GetBumpBondYPosition(G4int id) const;

  //////////////
  // Detector //
  //////////////
  G4bool GetStationIn(G4int station) {return fStationIn[station];}
  G4int GetNStation() {return fnStation;}

  G4String GetGigaTrackerSensitiveDetectorName() {return fGigaTrackerSensitiveDetectorName;};
  void SetGigaTrackerSensitiveDetectorName(G4String value){fGigaTrackerSensitiveDetectorName = value;};
  G4String GetGigaTrackerCollectionName() {return fGigaTrackerCollectionName;};
  void SetGigaTrackerCollectionName(G4String value) {fGigaTrackerCollectionName = value;};

  G4double GetGigaTrackerDetectorZPosition() {return fGigaTrackerDetectorZPosition;};
  void SetGigaTrackerDetectorZPosition(G4double value) {fGigaTrackerDetectorZPosition = value;};
 
  G4double GetGigaTrackerDetectorZLength() {return fGigaTrackerDetectorZLength;};
  void SetGigaTrackerDetectorZLength(G4double value){fGigaTrackerDetectorZLength = value;};
  G4double GetGigaTrackerDetectorXLength() {return fGigaTrackerDetectorXLength;};
  void SetGigaTrackerDetectorXLength(G4double value){fGigaTrackerDetectorXLength = value;};
  G4double GetGigaTrackerDetectorYLength() {return fGigaTrackerDetectorYLength;};
  void SetGigaTrackerDetectorYLength(G4double value){fGigaTrackerDetectorYLength = value;};
 
  /////////////////////
  // Sensor & pixels //
  /////////////////////
 
  // Sensor effective size
  G4double GetGigaTrackerSensorXLength(G4int station) {return fGigaTrackerSensorXLength[station];};
  void SetGigaTrackerSensorXLength(G4int station,G4double value){fGigaTrackerSensorXLength[station] = value;};
  G4double GetGigaTrackerSensorYLength(G4int station) {return fGigaTrackerSensorYLength[station];};
  void SetGigaTrackerSensorYLength(G4int station,G4double value){fGigaTrackerSensorYLength[station] = value;};
  G4double GetGigaTrackerSensorZLength(G4int station) {return fGigaTrackerSensorZLength[station];};
  void SetGigaTrackerSensorZLength(G4int station,G4double value){fGigaTrackerSensorZLength[station] = value;};

  // Sensor active area (pixels)
  G4double 	 GetGigaTrackerActiveSensorXLength(G4int station) {return fGigaTrackerActiveSensorXLength[station];}
  G4double	 GetGigaTrackerActiveSensorYLength(G4int station) {return fGigaTrackerActiveSensorYLength[station];}
  G4double	 GetGigaTrackerActiveSensorZLength(G4int station) {return fGigaTrackerActiveSensorZLength[station];}


  ////////////////
  // Bumb bonds //
  ////////////////

  G4double GetGigaTrackerBumpBondingRLength() {return fGigaTrackerBumpBondingRLength;};
  void SetGigaTrackerBumpBondingRLength(G4double value){fGigaTrackerBumpBondingRLength = value;};
  G4double GetGigaTrackerBumpBondingZLength() {return fGigaTrackerBumpBondingZLength;};
  void SetGigaTrackerBumpBondingZLength(G4double value){fGigaTrackerBumpBondingZLength = value;};
  G4ThreeVector GetGigaTrackerBumpBondingPosition(G4int station){return fGigaTrackerBumpBondingPosition[station];};
  void SetGigaTrackerBumpBondingPosition(G4int station, G4ThreeVector value) {fGigaTrackerBumpBondingPosition[station] = value;};

  ///////////////////
  // Readout chips //
  ///////////////////

  G4double GetGigaTrackerChipXLength(G4int station){return fGigaTrackerChipXLength[station];};
  void SetGigaTrackerChipXLength(G4int station,G4double value) {fGigaTrackerChipXLength[station] = value;};
  G4double GetGigaTrackerChipYLength(G4int station){return fGigaTrackerChipYLength[station];};
  void SetGigaTrackerChipYLength(G4int station,G4double value) {fGigaTrackerChipYLength[station] = value;};
  G4double GetGigaTrackerChipZLength(G4int station){
    if(station >= 0 && station < fnStationMax) {
      return fGigaTrackerChipZLength[station];
    } else {
      G4cerr << "[GigaTrackerGeometryParameters] Bad station ID (" << station << ")" << G4endl;
      return -1.0;
    }
  };
  void SetGigaTrackerChipZLength(G4double value, G4int station) {
    if(station >= 0 && station < fnStationMax) {
      fGigaTrackerChipZLength[station] = value; 
    } else {
      G4cerr << "[GigaTrackerGeometryParameters] Bad station ID (" << station << ")" << G4endl;
    }
  };
 
  G4double GetGigaTrackerChipXGap() {return fGigaTrackerChipXGap;};
  void SetGigaTrackerChipXGap(G4double value) {fGigaTrackerChipXGap = value;};
  G4ThreeVector GetGigaTrackerChipPosition(G4int station) {
 
    if(station >= 0 && station < fnStationMax) {
      return fGigaTrackerChipPosition[station];
    } else {

      G4cerr << "[GigaTrackerGeometryParameters] Bad station ID (" << station << ")" << G4endl;
      return G4ThreeVector(-1.0,-1.0,-1.0);

    }
  };
  void SetGigaTrackerChipPosition(G4ThreeVector value, G4int station) {

    if(station >= 0 && station < fnStationMax) {
      fGigaTrackerChipPosition[station] = value; 
    } else {
      G4cerr << "[GigaTrackerGeometryParameters] Bad station ID (" << station << ")" << G4endl;
    }
  };


  /////////////////////
  // Sensor assembly //
  /////////////////////

  G4double GetGigaTrackerSensorAssemblyXLength(G4int station) {
 
    if(station >= 0 && station < fnStationMax) {
      return fGigaTrackerSensorAssemblyXLength[station]; 
    } else {
      G4cerr << "[GigaTrackerGeometryParameters] Bad station ID (" << station << ")" << G4endl;
      return -1.0;
    }
  };
  void SetGigaTrackerSensorAssemblyXLength(G4double value, G4int station) {
 
    if(station >= 0 && station < fnStationMax) {
      fGigaTrackerSensorAssemblyXLength[station] = value; 
    } else {
      G4cerr << "[GigaTrackerGeometryParameters] Bad station ID (" << station << ")" << G4endl;
    }
  };

  G4double GetGigaTrackerSensorAssemblyYLength(G4int station) {
 
    if(station >= 0 && station < fnStationMax) {
      return fGigaTrackerSensorAssemblyYLength[station]; 
    } else {
      G4cerr << "[GigaTrackerGeometryParameters] Bad station ID (" << station << ")" << G4endl;
      return -1.0;
    }
  };
  void SetGigaTrackerSensorAssemblyYLength(G4double value, G4int station) {
 
    if(station >= 0 && station < fnStationMax) {
      fGigaTrackerSensorAssemblyYLength[station] = value; 
    } else {
      G4cerr << "[GigaTrackerGeometryParameters] Bad station ID (" << station << ")" << G4endl;
    }
  };
  G4double GetGigaTrackerSensorAssemblyZLength(G4int station) {
 
    if(station >= 0 && station < fnStationMax) {
      return fGigaTrackerSensorAssemblyZLength[station]; 
    } else {
      G4cerr << "[GigaTrackerGeometryParameters] Bad station ID (" << station << ")" << G4endl;
      return -1.0;
    }
  };
  void SetGigaTrackerSensorAssemblyZLength(G4double value, G4int station) {
 
    if(station >= 0 && station < fnStationMax) {
      fGigaTrackerSensorAssemblyZLength[station] = value; 
    } else {
      G4cerr << "[GigaTrackerGeometryParameters] Bad station ID (" << station << ")" << G4endl;
    }
  };
  G4ThreeVector GetGigaTrackerSensorPosition(G4int station){return fGigaTrackerSensorPosition[station];};
  void SetGigaTrackerSensorPosition(G4int station, G4ThreeVector value) {fGigaTrackerSensorPosition[station] = value;};

  // Pixels
  G4double GetGigaTrackerSmallPixelXLength() {return fGigaTrackerSmallPixelXLength;};
  void SetGigaTrackerSmallPixelXLength(G4double value) {fGigaTrackerSmallPixelXLength = value;};
  G4double GetGigaTrackerSmallPixelYLength() {return fGigaTrackerSmallPixelYLength;};
  void SetGigaTrackerSmallPixelYLength(G4double value) {fGigaTrackerSmallPixelYLength = value;};
  G4double GetGigaTrackerBigPixelXLength() {return fGigaTrackerBigPixelXLength;};
  void SetGigaTrackerBigPixelXLength(G4double value) {fGigaTrackerBigPixelXLength = value;};
  G4double GetGigaTrackerBigPixelYLength() {return fGigaTrackerBigPixelYLength;};
  void SetGigaTrackerBigPixelYLength(G4double value) {fGigaTrackerBigPixelYLength = value;};
  G4double GetGigaTrackerPixelZLength() {return fGigaTrackerPixelZLength;};
  void SetGigaTrackerPixelZLength(G4double value) {fGigaTrackerPixelZLength = value;};

  G4int GetGigaTrackerNumberOfPixels() {return fGigaTrackerNumberOfPixels;};
  void SetGigaTrackerNumberOfPixels(G4int value) {fGigaTrackerNumberOfPixels = value;};

  /////////
  // PCB //
  /////////
  G4double GetGigaTrackerPCBXLength() {return fGigaTrackerPCBXLength;}
  G4double GetGigaTrackerPCBYLength() {return fGigaTrackerPCBYLength;}
  G4double GetGigaTrackerPCBZLength() {return fGigaTrackerPCBZLength;}
  G4double GetGigaTrackerPCBHoleXLength() {return fGigaTrackerPCBHoleXLength;}
  G4double GetGigaTrackerPCBHoleYLength() {return fGigaTrackerPCBHoleYLength;}
  G4double GetGigaTrackerPCBHoleXOffset() {return fGigaTrackerPCBHoleXOffset;}
 
  /////////////
  // Station //
  /////////////

  G4double GetGigaTrackerStationXLength(G4int station) {return fGigaTrackerStationXLength[station];};
  void SetGigaTrackerStationXLength(G4int station,G4double value) {fGigaTrackerStationXLength[station] = value;};
  G4double GetGigaTrackerStationYLength(G4int station) {return fGigaTrackerStationYLength[station];};
  void SetGigaTrackerStationYLength(G4int station,G4double value) {fGigaTrackerStationYLength[station] = value;};
  G4double GetGigaTrackerStationZLength(G4int station) {return fGigaTrackerStationZLength[station];};
  void SetGigaTrackerStationZLength(G4int station,G4double value) {fGigaTrackerStationZLength[station] = value;};
 
  G4ThreeVector GetGigaTrackerStationPosition(G4int station){return fGigaTrackerStationPosition[station];};
  void SetGigaTrackerStationPosition(G4int station, G4ThreeVector value){fGigaTrackerStationPosition[station] = value;};

  /////////////
  // Magnets //
  /////////////

  ////////////////
  // MCB Magnet //
  ////////////////

  G4double GetGigaTrackerMCBMagnetFieldXLength() {return fGigaTrackerMCBMagnetFieldXLength;};
  void SetGigaTrackerMCBMagnetFieldXLength(G4double value) {fGigaTrackerMCBMagnetFieldXLength = value;};
  G4double GetGigaTrackerMCBMagnetFieldYLength() {return fGigaTrackerMCBMagnetFieldYLength;};
  void SetGigaTrackerMCBMagnetFieldYLength(G4double value) {fGigaTrackerMCBMagnetFieldYLength = value;};
  G4double GetGigaTrackerMCBMagnetFieldZLength() {return fGigaTrackerMCBMagnetFieldZLength;};
  void SetGigaTrackerMCBMagnetFieldZLength(G4double value) {fGigaTrackerMCBMagnetFieldZLength = value;};
  G4double GetGigaTrackerMCBMagnetBaseYLength() {return fGigaTrackerMCBMagnetBaseYLength;};
  void SetGigaTrackerMCBMagnetBaseYLength(G4double value) {fGigaTrackerMCBMagnetBaseYLength = value;};
  G4double GetGigaTrackerMCBMagnetGapXLength() {return fGigaTrackerMCBMagnetGapXLength;};
  void SetGigaTrackerMCBMagnetGapXLength(G4double value){fGigaTrackerMCBMagnetGapXLength = value;};
  G4double GetGigaTrackerMCBMagnetSideYLength() {return fGigaTrackerMCBMagnetSideYLength;};
  void SetGigaTrackerMCBMagnetSideYLength(G4double value) {fGigaTrackerMCBMagnetSideYLength = value;};
  G4double GetGigaTrackerMCBMagnetHatYLength() {return fGigaTrackerMCBMagnetHatYLength ;};
  void SetGigaTrackerMCBMagnetHatYLength(G4double value){fGigaTrackerMCBMagnetHatYLength = value;};
  G4double GetGigaTrackerMCBMagnetBeamYPos() {return fGigaTrackerMCBMagnetBeamYPos;};
  void SetGigaTrackerMCBMagnetBeamYPos(G4double value) {fGigaTrackerMCBMagnetBeamYPos = value;};
  G4double GetGigaTrackerMCBMagnetXLength() {return fGigaTrackerMCBMagnetXLength;};
  void SetGigaTrackerMCBMagnetXLength(G4double value) {fGigaTrackerMCBMagnetXLength = value;};
  G4double GetGigaTrackerMCBMagnetYLength() {return fGigaTrackerMCBMagnetYLength;};
  void SetGigaTrackerMCBMagnetYLength(G4double value) {fGigaTrackerMCBMagnetYLength = value;};
  G4double GetGigaTrackerMCBMagnetZLength() {return fGigaTrackerMCBMagnetZLength;};
  void SetGigaTrackerMCBMagnetZLength(G4double value) {fGigaTrackerMCBMagnetZLength = value;};
 
  ////////////////
  // MDX Magnet //
  ////////////////
 
  G4double GetGigaTrackerMDXMagnetFieldXLength() {return fGigaTrackerMDXMagnetFieldXLength;};
  void SetGigaTrackerMDXMagnetFieldXLength(G4double value) {fGigaTrackerMDXMagnetFieldXLength = value;};
  G4double GetGigaTrackerMDXMagnetFieldYLength() {return fGigaTrackerMDXMagnetFieldYLength;};
  void SetGigaTrackerMDXMagnetFieldYLength(G4double value) {fGigaTrackerMDXMagnetFieldYLength = value;};
  G4double GetGigaTrackerMDXMagnetFieldZLength() {return fGigaTrackerMDXMagnetFieldZLength;};
  void SetGigaTrackerMDXMagnetFieldZLength(G4double value) {fGigaTrackerMDXMagnetFieldZLength = value;};
  G4double GetGigaTrackerMDXMagnetGapXLength() {return fGigaTrackerMDXMagnetGapXLength;};
  void SetGigaTrackerMDXMagnetGapXLength(G4double value) {fGigaTrackerMDXMagnetGapXLength = value;};
  G4double GetGigaTrackerMDXMagnetGapYLength() {return fGigaTrackerMDXMagnetGapYLength;};
  void SetGigaTrackerMDXMagnetGapYLength(G4double value) {fGigaTrackerMDXMagnetGapYLength = value;};
  G4double GetGigaTrackerMDXMagnetXLength() {return fGigaTrackerMDXMagnetXLength;};
  void SetGigaTrackerMDXMagnetXLength(G4double value) {fGigaTrackerMDXMagnetXLength = value;};
  G4double GetGigaTrackerMDXMagnetYLength() {return fGigaTrackerMDXMagnetYLength;};
  void SetGigaTrackerMDXMagnetYLength(G4double value) {fGigaTrackerMDXMagnetYLength = value;};
  G4double GetGigaTrackerMDXMagnetZLength() {return fGigaTrackerMDXMagnetZLength;};
  void SetGigaTrackerMDXMagnetZLength(G4double value) {fGigaTrackerMDXMagnetZLength = value;};

  G4ThreeVector GetGigaTrackerMagnet1Position() {return fGigaTrackerMagnet1Position;};
  void SetGigaTrackerMagnet1Position(G4ThreeVector value) {fGigaTrackerMagnet1Position = value;};
  G4ThreeVector GetGigaTrackerMagnet2Position() {return fGigaTrackerMagnet2Position;};
  void SetGigaTrackerMagnet2Position(G4ThreeVector value) {fGigaTrackerMagnet2Position = value;};
  G4ThreeVector GetGigaTrackerMagnet3Position() {return fGigaTrackerMagnet3Position;};
  void SetGigaTrackerMagnet3Position(G4ThreeVector value) {fGigaTrackerMagnet3Position = value;};
  G4ThreeVector GetGigaTrackerMagnet4Position() {return fGigaTrackerMagnet4Position;};
  void SetGigaTrackerMagnet4Position(G4ThreeVector value) {fGigaTrackerMagnet4Position = value;};
  G4ThreeVector GetGigaTrackerMagnet5Position() {return fGigaTrackerMagnet5Position;};
  void SetGigaTrackerMagnet5Position(G4ThreeVector value) {fGigaTrackerMagnet5Position = value;};


  ///////////////////////////
  // Scraper (XCMV) magnet //
  ///////////////////////////

  G4ThreeVector GetGigaTrackerScraperMagnetPosition() {return fGigaTrackerScraperMagnetPosition;}
  void SetGigaTrackerScraperMagnetPosition(G4ThreeVector value){fGigaTrackerScraperMagnetPosition = value;}
  G4double GetGigaTrackerScraperMagnetZLength() {return fGigaTrackerScraperMagnetZLength;}
  void SetGigaTrackerScraperMagnetZLength(G4double value) {fGigaTrackerScraperMagnetZLength = value;}
  G4double GetGigaTrackerScraperMagnetApertureHalfWidth() {return fGigaTrackerScraperMagnetApertureHalfWidth;}
  void SetGigaTrackerScraperMagnetApertureHalfWidth(G4double value) {fGigaTrackerScraperMagnetApertureHalfWidth = value;} 
  G4double GetGigaTrackerScraperMagnetApertureHalfHeight() {return fGigaTrackerScraperMagnetApertureHalfHeight;}
  void SetGigaTrackerScraperMagnetApertureHalfHeight(G4double value) {fGigaTrackerScraperMagnetApertureHalfHeight = value;}
  G4double GetGigaTrackerScraperMagnetOverallHalfHeight() {return fGigaTrackerScraperMagnetOverallHalfHeight;}
  void SetGigaTrackerScraperMagnetOverallHalfHeight(G4double value) {fGigaTrackerScraperMagnetOverallHalfHeight = value;}
  G4double GetGigaTrackerScraperMagnetFieldStrength() {return fGigaTrackerScraperMagnetFieldStrength;}
  void SetGigaTrackerScraperMagnetFieldStrength(G4double value){fGigaTrackerScraperMagnetFieldStrength = value;}

  ////////////////
  // Collimator //
  ////////////////

  G4double GetGigaTrackerCollimatorOuterXLength() {return fGigaTrackerCollimatorOuterXLength;};
  void SetGigaTrackerCollimatorOuterXLength(G4double value) {fGigaTrackerCollimatorOuterXLength = value;};
  G4double GetGigaTrackerCollimatorOuterYLength() {return fGigaTrackerCollimatorOuterYLength;};
  void SetGigaTrackerCollimatorOuterYLength(G4double value) {fGigaTrackerCollimatorOuterYLength = value;};
  G4double GetGigaTrackerCollimatorInnerXLength() {return fGigaTrackerCollimatorInnerXLength;};
  void SetGigaTrackerCollimatorInnerXLength(G4double value) {fGigaTrackerCollimatorInnerXLength = value;};
  G4double GetGigaTrackerCollimatorInnerYLength() {return fGigaTrackerCollimatorInnerYLength;};
  void SetGigaTrackerCollimatorInnerYLength(G4double value) {fGigaTrackerCollimatorInnerYLength = value;};
  G4double GetGigaTrackerCollimatorZLength() {return fGigaTrackerCollimatorZLength;};
  void SetGigaTrackerCollimatorZLength(G4double value) {fGigaTrackerCollimatorZLength = value;};
 G4ThreeVector GetGigaTrackerCollimatorPosition() {return fGigaTrackerCollimatorPosition;};
  void SetGigaTrackerCollimatorPosition(G4ThreeVector value) {fGigaTrackerCollimatorPosition = value;};
  G4String GetGigaTrackerCollimatorGDMLPath() {return fGigaTrackerCollimatorGDML;}
  void SetGigaTrackerCollimatorGDMLPath(G4String name) {fGigaTrackerCollimatorGDML = name;}

  G4double GetGigaTrackerArchomatMagnetFieldStrength(G4int i) {return fGigaTrackerArchomatMagnetFieldStrength[i];}
  void SetGigaTrackerArchomatMagnetFieldStrength(G4double value, G4int i) {fGigaTrackerArchomatMagnetFieldStrength[i]=value;}
  
  G4double GetGigaTrackerTRIM5MagnetFieldStrength() {return fGigaTrackerTRIM5MagnetFieldStrength;}
  void SetGigaTrackerTRIM5MagnetFieldStrength(G4double value) {fGigaTrackerTRIM5MagnetFieldStrength = value;}

  G4String GetGigaTrackerCollimatorDesign()  {return fGigaTrackerCollimatorDesign;} 
  void SetGigaTrackerCollimatorDesign(G4String name)  {fGigaTrackerCollimatorDesign = name;} 
 

  ///////////////////
  // Cooling Plate //
  ///////////////////
	
  G4double GetCoolingPlateXLength(int station) {return fCoolingPlateXLength[station];}
  G4double GetCoolingPlateYLength(int station) {return fCoolingPlateYLength[station];}
  G4double GetCoolingPlateZLength(int station) {
    if(station >= 0 && station < fnStationMax) return fCoolingPlateZLength[station];
    else {
      G4cerr << "[CoolingPlateZLength] Bad station ID (" << station << ")" << G4endl;
      return -1.0;
    }
  }

 
  G4double GetCoolingPlateTopShoulderXLength() {return fCoolingPlateTopShoulderXLength;}
  G4double GetCoolingPlateTopHollowXLength() {return fCoolingPlateTopHollowXLength;}
  G4double GetCoolingPlateTopShoulderYLength() {return fCoolingPlateTopShoulderYLength;}
  G4double GetCoolingPlateTopHollowYLength() {return fCoolingPlateTopHollowYLength;}
  G4double GetCoolingPlateTopDepth(int station) {
    if(station >= 0 && station < fnStationMax) return fCoolingPlateTopDepth[station];
    else {
      G4cerr << "[CoolingPlateTopDepth] Bad station ID (" << station << ")" << G4endl;
      return -1.0;
    }
  }

 
  G4double GetCoolingPlateBottomShoulderXLength() {return fCoolingPlateBottomShoulderXLength;}
  G4double GetCoolingPlateBottomHollowXLength() {return fCoolingPlateBottomHollowXLength;}
  G4double GetCoolingPlateBottomShoulderYLength() {return fCoolingPlateBottomShoulderYLength;}
  G4double GetCoolingPlateBottomHollowYLength() {return fCoolingPlateBottomHollowYLength;}
  G4double GetCoolingPlateBottomDepth(int station) {
    if(station >= 0 && station < fnStationMax) return fCoolingPlateBottomDepth[station];
    else {
      G4cerr << "[CoolingPlateBottomDepth] Bad station ID (" << station << ")" << G4endl;
      return -1.0;
    }
  } 
  G4double GetCoolingPlateChannelsEnvelopeXLength() {return fCoolingPlateChannelsEnvelopeXLength;}
  G4double GetCoolingPlateChannelsEnvelopeYLength() {return fCoolingPlateChannelsEnvelopeYLength;}
  G4double GetCoolingPlateChannelsEnvelopeZLength() {return fCoolingPlateChannelsEnvelopeZLength;}
 
  G4double GetCoolingPlateChannelsXOffset() {return fCoolingPlateChannelsXOffset;}
  G4double GetCoolingPlateChannelsDepth() {return fCoolingPlateChannelsDepth;}
 
  G4double GetCoolingPlateChannelHalfWallXLength() {return fCoolingPlateChannelHalfWallXLength;}
  G4double GetCoolingPlateChannelCoolantXLength() {return fCoolingPlateChannelCoolantXLength;}
 
  ////////////////
  // Glue layer //
  ////////////////
 
  G4double GetGigaTrackerGlueLayerZLength() {return fGigaTrackerGlueLayerZLength;}

private:
  GigaTrackerParameterTools * fParameters;

  G4String fGigaTrackerSensitiveDetectorName;
  G4String fGigaTrackerCollectionName;

  G4int fnStation = 3;
  G4bool fStationIn[fnStationMax];

  G4double fWorldZLength;
  G4double fWorldXLength;
  G4double fWorldYLength;

  G4double fGigaTrackerDetectorZPosition;

  G4double fGigaTrackerDetectorZLength;
  G4double fGigaTrackerDetectorXLength;
  G4double fGigaTrackerDetectorYLength;

  G4ThreeVector fGigaTrackerStationPosition[fnStationMax];
  G4double fGigaTrackerStationXLength[fnStationMax];
  G4double fGigaTrackerStationYLength[fnStationMax];
  G4double fGigaTrackerStationZLength[fnStationMax];

  // Sensors
  G4double fGigaTrackerSensorAssemblyXLength[fnStationMax]; 
  G4double fGigaTrackerSensorAssemblyYLength[fnStationMax];
  G4double fGigaTrackerSensorAssemblyZLength[fnStationMax];

  G4double fGigaTrackerSensorXLength[fnStationMax];
  G4double fGigaTrackerSensorYLength[fnStationMax];
  G4double fGigaTrackerSensorZLength[fnStationMax];
 
  G4double fGigaTrackerActiveSensorXLength[fnStationMax];
  G4double fGigaTrackerActiveSensorYLength[fnStationMax];
  G4double fGigaTrackerActiveSensorZLength[fnStationMax];

  G4ThreeVector fGigaTrackerSensorPosition[fnStationMax];

  G4int fGigaTrackerSensorNColumns;
  G4int fGigaTrackerSensorNRows;
  G4int fGigaTrackerChipNColumns;
  G4int fGigaTrackerChipNRows;

  // Bump bonds
  G4double fGigaTrackerBumpBondingRLength;
  G4double fGigaTrackerBumpBondingZLength;
  G4double fGigaTrackerBumpBondingXOffset;
  G4double fGigaTrackerBumpBondingYOffset;

  G4double fGigaTrackerBumpBondOffset;
  G4double fGigaTrackerBumpBondBigOffset;

  G4ThreeVector fGigaTrackerBumpBondingPosition[fnStationMax];

  // Chips
  G4double fGigaTrackerChipXLength[fnStationMax];
  G4double fGigaTrackerChipYLength[fnStationMax];
  G4double fGigaTrackerChipZLength[fnStationMax];
  G4double fGigaTrackerChipXGap;

  G4ThreeVector fGigaTrackerChipPosition[fnStationMax];

  // Depreciated
  G4ThreeVector fGigaTrackerSupportPosition[fnStationMax];
  G4ThreeVector fGigaTrackerSupportLength[fnStationMax];

  // Pixels
  G4double fGigaTrackerSmallPixelXLength;
  G4double fGigaTrackerSmallPixelYLength;
  G4double fGigaTrackerBigPixelXLength;
  G4double fGigaTrackerBigPixelYLength;
  G4double fGigaTrackerPixelZLength;
  G4int fGigaTrackerNumberOfPixels;

  // Cooling Plates
  G4double fCoolingPlateXLength[fnStationMax];
  G4double fCoolingPlateYLength[fnStationMax];
  G4double fCoolingPlateZLength[fnStationMax];

  G4double fCoolingPlateTopShoulderXLength;
  G4double fCoolingPlateTopHollowXLength;
  G4double fCoolingPlateTopShoulderYLength;
  G4double fCoolingPlateTopHollowYLength;
  G4double fCoolingPlateTopDepth[fnStationMax];

  G4double fCoolingPlateBottomShoulderXLength;
  G4double fCoolingPlateBottomHollowXLength;
  G4double fCoolingPlateBottomShoulderYLength;
  G4double fCoolingPlateBottomHollowYLength;
  G4double fCoolingPlateBottomDepth[fnStationMax];

  G4double fCoolingPlateChannelsEnvelopeXLength;
  G4double fCoolingPlateChannelsEnvelopeYLength;
  G4double fCoolingPlateChannelsEnvelopeZLength;

  G4double fCoolingPlateChannelsXOffset;
  G4double fCoolingPlateChannelsDepth;

  G4double fCoolingPlateChannelCoolantXLength;
  G4double fCoolingPlateChannelHalfWallXLength;

  // Glue
  G4double fGigaTrackerGlueLayerZLength;

  // PCB
  G4double fGigaTrackerPCBXLength; 
  G4double fGigaTrackerPCBYLength;
  G4double fGigaTrackerPCBZLength;
  G4double fGigaTrackerPCBHoleXLength;
  G4double fGigaTrackerPCBHoleYLength;
  G4double fGigaTrackerPCBHoleXOffset;

  // Magnets
  G4double fGigaTrackerMCBMagnetXLength;
  G4double fGigaTrackerMCBMagnetYLength;
  G4double fGigaTrackerMCBMagnetZLength;
  G4double fGigaTrackerMCBMagnetFieldXLength;
  G4double fGigaTrackerMCBMagnetFieldYLength;
  G4double fGigaTrackerMCBMagnetFieldZLength;
  G4double fGigaTrackerMCBMagnetBaseYLength;
  G4double fGigaTrackerMCBMagnetGapXLength;
  G4double fGigaTrackerMCBMagnetSideYLength;
  G4double fGigaTrackerMCBMagnetHatYLength;
  G4double fGigaTrackerMCBMagnetBeamYPos; 

  G4double fGigaTrackerMDXMagnetXLength;
  G4double fGigaTrackerMDXMagnetYLength;
  G4double fGigaTrackerMDXMagnetZLength;
  G4double fGigaTrackerMDXMagnetFieldXLength;
  G4double fGigaTrackerMDXMagnetFieldYLength;
  G4double fGigaTrackerMDXMagnetFieldZLength;
  G4double fGigaTrackerMDXMagnetGapXLength;
  G4double fGigaTrackerMDXMagnetGapYLength;

  G4ThreeVector fGigaTrackerScraperMagnetPosition;
  G4double fGigaTrackerScraperMagnetZLength;
  G4double fGigaTrackerScraperMagnetApertureHalfWidth;
  G4double fGigaTrackerScraperMagnetApertureHalfHeight;
  G4double fGigaTrackerScraperMagnetOverallHalfHeight;
  G4double fGigaTrackerScraperMagnetFieldStrength;

  G4double fGigaTrackerCollimatorOuterXLength;
  G4double fGigaTrackerCollimatorOuterYLength;
  G4double fGigaTrackerCollimatorInnerXLength;
  G4double fGigaTrackerCollimatorInnerYLength;
  G4double fGigaTrackerCollimatorZLength;
  G4String fGigaTrackerCollimatorDesign;   
  G4String fGigaTrackerCollimatorGDML;

  G4ThreeVector fGigaTrackerMagnet1Position;
  G4ThreeVector fGigaTrackerMagnet2Position;
  G4ThreeVector fGigaTrackerMagnet3Position;
  G4ThreeVector fGigaTrackerMagnet4Position;
  G4ThreeVector fGigaTrackerMagnet5Position;

  G4ThreeVector fGigaTrackerCollimatorPosition;

  G4double fGigaTrackerArchomatMagnetFieldStrength[4];
  G4double fGigaTrackerTRIM5MagnetFieldStrength;
};

#endif
