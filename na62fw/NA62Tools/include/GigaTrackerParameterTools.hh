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
// Important!: all magnet and station position parameter values given in meters (m)!
// Important!: all other parameter values given in meters (mm)!
// Positions taken from http://eagroup.web.cern.ch/eagroup/beatch/k12hika+P.txt
// --------------------------------------------------------------
//

#ifndef GigaTrackerParameterTools_H
#define GigaTrackerParameterTools_H 1

#include "TVector3.h"

#include <iostream>
#include <map>
#include <cstdlib>
#include <sstream>

class GigaTrackerParameterTools{

private:
  GigaTrackerParameterTools(int runID = 0, bool isMC = false);
  ~GigaTrackerParameterTools() {}
  static GigaTrackerParameterTools* fInstance;

  // .dat file names from Conditions
  TString fStationFile;
  TString fTZeroFile;
  TString fTWalkFile;
  TString fXYFile;
  TString fOffsetFile;
  TString fRotationFile;
  TString fMomentumScaleFile;
  TString fSensorLFile;
  TString fCoolPlatesFile;
  TString fPCBParFile;
  TString fCollimatorFile;
  
  //------------- Generals ----------------//
  bool fIsMC      = false;
  int  fNStations = 3; // default 3 statiosn, there could be up to 4 stations, values will always be in arrays of 4 entries, 
                       // if station not used, values in .dat should be set to 0
  const int fNStationsMax = 4;

  // Sensitive detector
  TString fGigaTrackerSensitiveDetectorName = "/GigaTracker";
  TString fGigaTrackerCollectionName        = "GigaTrackerCollection";

  TVector3 fWorldLength                     = TVector3(10000.0, 10000.0, 26000.0); 

  double fGigaTrackerStation1VXBOEndPosition      =  79440.0;
  double fGigaTrackerStation3BeforeCHANTIPosition = 102420.0; // correct would be: 102.428; check!

  TVector3 fGigaTrackerStationPositionRaw[4]; // values set in .cc
  // replaces fGigaTrackerStationXLength
  TVector3 fGigaTrackerStationLength[4];      // values set in .cc

  //-------------- Magnets ---------------//
  // MCB =  magnet 1 = Archromat
  TVector3 fGigaTrackerMCBMagnetRawPosition[4]; // values set in .cc
  TVector3 fGigaTrackerMCBMagnetLength         = TVector3(1250.0, 1246.0, 2500.0);
  TVector3 fGigaTrackerMCBMagnetFieldLength    = TVector3(  80.0,  160.0, 2500.0);
  double   fGigaTrackerMCBMagnetBaseYLength    = 300.0;
  double   fGigaTrackerMCBMagnetGapXLength     = 100.0;
  double   fGigaTrackerMCBMagnetSideYLength    = 646.0;
  double   fGigaTrackerMCBMagnetHatYLength     = 300.0;
  // Beam pos (from the *bottom* of the base)
  double fGigaTrackerMCBMagnetBeamYPos         = 764.0; 
  // replace fGigaTrackerArchomatMagnetFieldStrength
  double fGigaTrackerMCBMagnetFieldStrength[4] = {-1.6678, 1.6678, 1.6678, -1.6678}; // tesla
  double fMCBGTK4PostionZ                      = 82660.0;

  //MDX = magnet 5 = TRIM5
  TVector3 fGigaTrackerMDXMagnetLength         = TVector3(600.0, 600.0,  400.0);
  TVector3 fGigaTrackerMDXMagnetFieldLength    = TVector3( 80.0, 100.0,  400.0);
  TVector3 fGigaTrackerMDXMagnetRawPosition    = TVector3(  0.0,   0.0, 102000);
  TVector3 fGigaTrackerMDXMagnetGapLength      = TVector3(100.0,100.0,0.0);
  double  fGigaTrackerMDXMagnetFieldStrength   = -0.7505 ;//tesla

  //Scraper
  TVector3 fGigaTrackerScraperMagnetRawPosition      = TVector3(0.0, -50.0, 92310); // was 89.810 in MC
  //replaces fGigaTrackerScraperMagnetZLength    
  TVector3 fGigaTrackerScraperMagnetLength           = TVector3(0.0, 0.0, 5000.0); 
  double fGigaTrackerScraperMagnetApertureHalfWidth  = 140.0; //--> into length!!!!!!!!!
  double fGigaTrackerScraperMagnetApertureHalfHeight =  30.0; //--> into length!!!!!!!!!
  double fGigaTrackerScraperMagnetOverallHalfHeight  = 380.0; //--> into length!!!!!!!!!
  double fGigaTrackerScraperMagnetFieldStrength      =   1.8; // tesla

  //------------- Sensors ----------------//
  int fGigaTrackerSensorNColumns = 200;
  int fGigaTrackerSensorNRows    = 90;
  int fGigaTrackerChipNColumns   = 40;
  int fGigaTrackerChipNRows      = 45;

  //------------- Bonding ----------------//  
  //Bump bonding stays over time ?????
  TVector3 fGigaTrackerBumpBondingOffset = TVector3(50.0e-3,50.0e-3,0.0);
  double fGigaTrackerBumpBondPixelOffset = 100.0e-3;
  double fGigaTrackerBumpBondBigOffset   = -50.0e-3;    

  //------------- Chips ----------------//
  // replaces fGigaTrackerChipXLength, fGigaTrackerChipYLength,fGigaTrackerChipZLength[3]
  TVector3 fGigaTrackerChipLength[4]; // values set in .cc
  double fGigaTrackerChipGap = 0.1;   // replace fGigaTrackerChipXGap
    
  // replaces fGigaTrackerSmallPixelXLength, fGigaTrackerSmallPixelYLength,fGigaTrackerPixelZLength
  TVector3 fGigaTrackerSmallPixelLength = TVector3(300.0e-3, 300.0e-3, 200.0e-3);
  // replaces fGigaTrackerBigPixelXLength, fGigaTrackerBigPixelYLength,fGigaTrackerPixelZLength
  TVector3 fGigaTrackerBigPixelLength   = TVector3(400.0e-3, 300.0e-3, 200.0e-3);
  int fGigaTrackerNumberOfPixels        = 18000;


  //------------- Support ---------------- NOT USED!//
  //replaces fGigaTrackerSupportXLength, fGigaTrackerSupportYLength fGigaTrackerSupportZLength
  // TVector3 fGigaTrackerSupportLength[4]; // values set in .cc
  

  //------------- For reconstruction ----------------//
  // station time resolution used to compute the chi2
  double fSigmaT[4]         = {0.2,0.2,0.2,0.2};
  // to make the hit clusters
  double fClusterTimeWindow = 5.0;  
  // window around the trigger time in which we take hits to reconstruct a track
  double fTimeWindow        = 5.0;
  // Digitizer
  double fQcut              = 0.7;
  double fQsaturation       = 18.0;
  double fToTFix            = 8.0;

  //----------------------------- parameters that change with time, will be read from .dat or calculated values ---------------------//
  
  //----------------- To be read from file ----------///
  
  //----------- Stations ------------//
  // up the GTK installed such that the sensor face the beam of the dump
  // (in this case the pixel Id to position is changed
  bool fGTKSensorUpstream[4] = {0,0,0,0};
  bool fStationIn[4]         = {1,1,1,0};
 
  //-------------- Misalignment --------------//  
  TVector3 fGigaTrackerStationMisalignment[4]; // misalingment from X-Y correction files
  TVector3 fGigaTrackerStationOffset[4];       // station poistion offset measured via beam spot

  //-------------- Magnets ------------------//
  // Collimator = TCX (mid 2017-2018) and XCHV101101 COLL 6 / 7 before
  // replaces fGigaTrackerCollimatorOuterXLength and fGigaTrackerCollimatorZLength
  TVector3 fGigaTrackerCollimatorOuterLength = TVector3(100.0 , 100.0, 1000.0); // new collimator: 1.2 in z
  //replaces fGigaTrackerCollimatorInnerXLength and fGigaTrackerCollimatorZLength
  TVector3 fGigaTrackerCollimatorInnerLength = TVector3(66.0, 33.0,   1000.0); // CHECK!!!!
  TVector3 fGigaTrackerCollimatorRawPosition = TVector3(0.0,   0.0, 101255.0);
  TString fGigaTrackerCollimatorDesign       = "2016";
  TString fGigaTrackerCollimatorGDML         = ""; //location of GDML file for collimator used in 2018  

  //--------------- Sensors ------------//
  //replaces fGigaTrackerSensorXLength,fGigaTrackerSensorYLength,fGigaTrackerSensorZLength and fGigaTrackerActiveSensorXLength
  TVector3 fGigaTrackerSensorLength[4];            // values set in .cc 
  TVector3 fGigaTrackerActiveSensorLength[4];      // values set in .cc 
  TVector3 fGigaTrackerSensorAddAssemblyLength[4]; // values set in .cc // ! may not be needed if the same 

  //------------- PCB ----------------//
  // replaces fGigaTrackerPCBXLength,fGigaTrackerPCBYLength,fGigaTrackerPCBZLength
  TVector3 fGigaTrackerPCBLength     = TVector3(270.0, 146.0, 2.0); 
  // replaces fGigaTrackerPCBHoleXLength, fGigaTrackerPCBHoleYLength
  TVector3 fGigaTrackerPCBHoleLength = TVector3(73.0, 43.0, 0.0);
  double fGigaTrackerPCBHoleXOffset  = 18.13;    

  //--------------- Cooling plates ------------//
  // Cooling Plate : z = CoolingPlateThickness1 from .db
  // replaces fCoolingPlateXLength,fCoolingPlateYLength fCoolingPlateZLength
  TVector3 fCoolingPlateLength[4];//values set in .cc 
  
  // replaces fCoolingPlateTopShoulderXLength
  TVector3 fCoolingPlateTopShoulderLength = TVector3(65.08, 41.5, 0.0);
  // replaces fCoolingPlateTopHollowXLength  
  TVector3 fCoolingPlateTopHollowLength   = TVector3(64.87, 41.29, 0.0);
  double fCoolingPlateTopDepth[4]         = {0.08, 0.08, 0.08, 0.08};

  // replaces fCoolingPlateBottomShoulderXLength
  TVector3 fCoolingPlateBottomShoulderLength = TVector3(65.8, 32.0, 0.0);
  // replaces fCoolingPlateBottomHollowXLength 
  TVector3 fCoolingPlateBottomHollowLength   = TVector3(64.27, 31.19, 0.0);
  double fCoolingPlateBottomDepth[4]         = {0.29, 0.29, 0.29, 0.29};
     
  // replaces fCoolingPlateChannelsEnvelopeXLength;, fCoolingPlateChannelsEnvelopeYLength;, fCoolingPlateChannelsEnvelopeZLength;
  TVector3 fCoolingPlateChannelsEnvelopeLength = TVector3(61.6, 41.29, 70.0e-3);
  double fCoolingPlateChannelsXOffset          = 1.73 - 100*1.e-3; // mm; // - Half channel wall ! 
  double fCoolingPlateChannelsDepth            = 200.0e-3;
  double fCoolingPlateChannelCoolantXLength    = 200.0e-3;
  double fCoolingPlateChannelHalfWallXLength   = 100.0e-3;
  
  //------------------- Glue ---------------//
  double fGigaTrackerGlueLayerZLength = 30.0e-3;

  //---------------- Bumper bonds ----------//
  double fGigaTrackerBumpBondingRLength = 10.0e-3;
  double fGigaTrackerBumpBondingZLength = 25.0e-3; 
  
  //----------------------------- To be calculated ---------------------------//       
  TVector3 fGigaTrackerStationPositionMC[4];   // values set in .cc 
  TVector3 fGigaTrackerStationPositionCorr[4]; // values set in .cc 
  double fGigaTrackerDetectorZPosition = 0.0;
  // replaces fGigaTrackerDetectorXLength
  TVector3 fGigaTrackerResponseRegion;
  
  //-------------- Magnets ------------------//
  // replacesfGigaTrackerMagnet1Position
  TVector3 fGigaTrackerMCBMagnetPosition[4]; // values set in .cc 
  // replaces fGigaTrackerMagnet5Position
  TVector3 fGigaTrackerMDXMagnetPosition; 
  TVector3 fGigaTrackerScraperMagnetPosition; 
  TVector3 fGigaTrackerCollimatorPosition;

  //----------------- Support --------------NOT USED!//
  // replaces   TVector3 fGigaTrackerSupportPosition[4]; 
  TVector3 fGigaTrackerSupportPosition[4]; // values set in .cc
 
  //--------------- Sensors ---------------//
  // replaces fGigaTrackerSensorAssemblyXLength fGigaTrackerSensorAssemblyYLength fGigaTrackerSensorAssemblyZLength[3]
  TVector3 fGigaTrackerSensorAssemblyLength[4]; 
  TVector3 fGigaTrackerSensorPosition[4];
 
  //--------------- Bump Bondings --------//
  TVector3 fGigaTrackerBumpBondingPosition[4];
  
  //------------------- Chips -----------//
  // replaces   TVector3 fGigaTrackerChipPosition[3];
  TVector3 fGigaTrackerChipPosition[4];


  //----------------------------------- Setters ----------------------------------------------//
  //--------------- To be calculated -------------//
  void SetGigaTrackerStationPositionMC();
  void SetGigaTrackerStationPositionCorrected();
  void SetGigaTrackerDetectorZPosition();
  void SetGigaTrackerResponseRegion();
    
  //Magnets
  void SetGigaTrackerMCBMagnetsPosition();
  void SetGigaTrackerMDXMagnetPosition();
  void SetGigaTrackerScraperMagnetPosition();
  void SetGigaTrackerCollimatorPosition();

  //Support --NOT USED
  // void SetGigaTrackerSupportPosition();

  //Sensors
  void SetGigaTrackerSensorAssemblyLength(); 
  void SetGigaTrackerSensorPosition();

  //BumpBondings
  void SetGigaTrackerBumpBondingPosition();
 
  //Chips
  void SetGigaTrackerChipPosition();

  //---------- others ---------//
  // this is the station time resolution used to compute the chi2
  // Commented out until needed
  // void SetSigmaT(const int station, double value)                    { fSigmaT[station] = value;                           }
  // void SetNStations(int value)                                       { fNStations       = value;                           }

public:

  static GigaTrackerParameterTools* GetInstance(int runID = 0, bool isMC = false);

  //----------------------------------- Getters ----------------------------------------------//
  //--------------------------- Values defined in this class ---------------//
  //------------- Generals ----------------//
  int      GetNStations()                                            { return fNStations;                                  }
  TString  GetGigaTrackerSensitiveDetectorName()                     { return fGigaTrackerSensitiveDetectorName;           }
  TString  GetGigaTrackerCollectionName()                            { return fGigaTrackerCollectionName;                  } 
  TVector3 GetWorldLength()                                          { return fWorldLength;                                }
  
  //-----------------Station position----------------
  double   GetGigaTrackerStation1VXBOEndPosition()                   { return fGigaTrackerStation1VXBOEndPosition;         }
  double   GetGigaTrackerStation3BeforeCHANTIPosition()              { return fGigaTrackerStation3BeforeCHANTIPosition;    }
  TVector3 GetGigaTrackerStationPositionRaw(const int station)       { return fGigaTrackerStationPositionRaw[station];     }
  TVector3 GetGigaTrackerStationLength(const int station)            { return fGigaTrackerStationLength[station];          }

  //-------------- Magnets ---------------//
  //MCB(4A,4B,5,6) = Archromats
  TVector3 GetGigaTrackerMCBMagnetLength()                           { return fGigaTrackerMCBMagnetLength;                 }
  TVector3 GetGigaTrackerMCBMagnetFieldLength()                      { return fGigaTrackerMCBMagnetFieldLength;            }
  TVector3 GetGigaTrackerMCBMagnetRawPosition(const int magnetN)     { return fGigaTrackerMCBMagnetRawPosition[magnetN];   }
  
  double   GetGigaTrackerMCBMagnetBaseYLength()                      { return fGigaTrackerMCBMagnetBaseYLength;            }
  double   GetGigaTrackerMCBMagnetGapXLength()                       { return fGigaTrackerMCBMagnetGapXLength;             }
  double   GetGigaTrackerMCBMagnetSideYLength()                      { return fGigaTrackerMCBMagnetSideYLength;            }
  double   GetGigaTrackerMCBMagnetHatYLength()                       { return fGigaTrackerMCBMagnetHatYLength;             }
  double   GetGigaTrackerMCBMagnetBeamYPos()                         { return fGigaTrackerMCBMagnetBeamYPos;               }
  double   GetGigaTrackerMCBMagnetFieldStrength(const int magnetN)   { return fGigaTrackerMCBMagnetFieldStrength[magnetN]; }

  //MDX = TRIM5
  TVector3 GetGigaTrackerMDXMagnetLength()                           { return fGigaTrackerMDXMagnetLength;                 }
  TVector3 GetGigaTrackerMDXMagnetFieldLength()                      { return fGigaTrackerMDXMagnetFieldLength;            } 
  TVector3 GetGigaTrackerMDXMagnetRawPosition()                      { return fGigaTrackerMDXMagnetRawPosition;            }
  TVector3 GetGigaTrackerMDXMagnetGapLength()                        { return fGigaTrackerMDXMagnetGapLength;              } 
  double   GetGigaTrackerMDXMagnetFieldStrength()                    { return fGigaTrackerMDXMagnetFieldStrength;          }
  
  //Scraper
  TVector3 GetGigaTrackerScraperMagnetRawPosition()                  { return fGigaTrackerScraperMagnetRawPosition;        }
  TVector3 GetGigaTrackerScraperMagnetLength()                       { return fGigaTrackerScraperMagnetLength;             } 
  double   GetGigaTrackerScraperMagnetApertureHalfWidth()            { return fGigaTrackerScraperMagnetApertureHalfWidth;  }
  double   GetGigaTrackerScraperMagnetApertureHalfHeight()           { return fGigaTrackerScraperMagnetApertureHalfHeight; }
  double   GetGigaTrackerScraperMagnetOverallHalfHeight()            { return fGigaTrackerScraperMagnetOverallHalfHeight;  }
  double   GetGigaTrackerScraperMagnetFieldStrength()                { return fGigaTrackerScraperMagnetFieldStrength;      }

  //Collimator
  TVector3 GetGigaTrackerCollimatorOuterLength()                     { return fGigaTrackerCollimatorOuterLength;           }
  TVector3 GetGigaTrackerCollimatorInnerLength()                     { return fGigaTrackerCollimatorInnerLength;           }
  TVector3 GetGigaTrackerCollimatorRawPosition()                     { return fGigaTrackerCollimatorRawPosition;           }
  TString  GetGigaTrackerCollimatorDesign()                           { return fGigaTrackerCollimatorDesign;               }
  TString  GetGigaTrackerCollimatorGDML()                            { return fGigaTrackerCollimatorGDML;                  }

  //Sensors
  int      GetGigaTrackerSensorNColumns()                            { return fGigaTrackerSensorNColumns;                  }
  int      GetGigaTrackerSensorNRows()                               { return fGigaTrackerSensorNRows;                     }
  int      GetGigaTrackerChipNColumns()                              { return fGigaTrackerChipNColumns;                    }
  int      GetGigaTrackerChipNRows()                                 { return fGigaTrackerChipNRows;                       }

  //Bump Bonding
  TVector3 GetGigaTrackerBumpBondingOffset()                         { return fGigaTrackerBumpBondingOffset;               }
  double   GetGigaTrackerBumpBondPixelOffset()                       { return fGigaTrackerBumpBondPixelOffset;             }  
  double   GetGigaTrackerBumpBondBigOffset()                         { return fGigaTrackerBumpBondBigOffset;               }
    
  //Chips
  TVector3 GetGigaTrackerChipLength(const int station)               { return fGigaTrackerChipLength[station];             }
  double   GetGigaTrackerChipGap()                                   { return fGigaTrackerChipGap;                         } 
    
  //Pixels
  TVector3 GetGigaTrackerSmallPixelLength()                          { return fGigaTrackerSmallPixelLength;                } 
  TVector3 GetGigaTrackerBigPixelLength()                            { return fGigaTrackerBigPixelLength;                  }
  int      GetGigaTrackerNumberOfPixels()                            { return fGigaTrackerNumberOfPixels;                  }
    
  //Support -- NOT USED!
  // TVector3 GetGigaTrackerSupportLength(const int station)            { return fGigaTrackerSupportLength[station];          } 
 
  //For reconstruction
  double   GetClusterTimeWindow()                                    { return fClusterTimeWindow;                          } 
  double   GetTimeWindow()                                           { return fTimeWindow;                                 } 

  //this is the station time resolution used to compute the chi2
  double   GetSigmaT(const int station)                              { return fSigmaT[station];                            }

  //-------------------------- Values to be read from file -------------------------///
  bool     GetStationIn(const int station)                           { return fStationIn[station];                         }
  bool     GetGTKSensorUpstream(const int station)                   { return fGTKSensorUpstream[station];                 }
    
  //Misalignment
  TVector3 GetGigaTrackerStationMisalignment(const int station)      { return fGigaTrackerStationMisalignment[station];    }
  TVector3 GetGigaTrackerStationOffset(const int station)            { return fGigaTrackerStationOffset[station];          }

  //Sensors    
  TVector3 GetGigaTrackerSensorLength(const int station)             { return fGigaTrackerSensorLength[station];           }
  TVector3 GetGigaTrackerActiveSensorLength(const int station)       { return fGigaTrackerActiveSensorLength[station];     } 

  //PCB
  TVector3 GetGigaTrackerPCBLength()                                 { return fGigaTrackerPCBLength;                       }
  TVector3 GetGigaTrackerPCBHoleLength()                             { return fGigaTrackerPCBHoleLength;                   }
  double   GetGigaTrackerPCBHoleXOffset()                            { return fGigaTrackerPCBHoleXOffset;                  }

  //Cooling plates
  TVector3 GetCoolingPlateLength(const int station)                  { return fCoolingPlateLength[station];                }    
  TVector3 GetCoolingPlateTopShoulderLength()                        { return fCoolingPlateTopShoulderLength;              }
  TVector3 GetCoolingPlateTopHollowLength()                          { return fCoolingPlateTopHollowLength;                }    
  TVector3 GetCoolingPlateBottomShoulderLength()                     { return fCoolingPlateBottomShoulderLength;           }
  TVector3 GetCoolingPlateBottomHollowLength()                       { return fCoolingPlateBottomHollowLength;             }
  TVector3 GetCoolingPlateChannelsEnvelopeLength()                   { return fCoolingPlateChannelsEnvelopeLength;         }
  double   GetCoolingPlateTopDepth(const int station)                { return fCoolingPlateTopDepth[station];              }
  double   GetCoolingPlateBottomDepth(const int station)             { return fCoolingPlateBottomDepth[station];           }
  double   GetCoolingPlateChannelsXOffset()                          { return fCoolingPlateChannelsXOffset;                }
  double   GetCoolingPlateChannelsDepth()                            { return fCoolingPlateChannelsDepth;                  }    
  double   GetCoolingPlateChannelCoolantXLength()                    { return fCoolingPlateChannelCoolantXLength;          }
  double   GetCoolingPlateChannelHalfWallXLength()                   { return fCoolingPlateChannelHalfWallXLength;         }
    
  //Bump bonds
  double   GetGigaTrackerBumpBondingRLength()                        { return fGigaTrackerBumpBondingRLength;              }
  double   GetGigaTrackerBumpBondingZLength()                        { return fGigaTrackerBumpBondingZLength;              }

  //Glue
  double   GetGigaTrackerGlueLayerZLength()                          { return fGigaTrackerGlueLayerZLength;                }

  //---------------------------- Values to be calculated ----------------------------------//
  double   GetGigaTrackerDetectorZPosition()                         { return fGigaTrackerDetectorZPosition;               }
  TVector3 GetGigaTrackerStationPositionMC(const int station)        { return fGigaTrackerStationPositionMC[station];      }
  TVector3 GetGigaTrackerStationPositionCorrected(const int station) { return fGigaTrackerStationPositionCorr[station];    } 
  TVector3 GetGigaTrackerResponseRegion()                            { return fGigaTrackerResponseRegion;                  }
      
  //Magnets
  TVector3 GetGigaTrackerMCBMagnetPosition(const int magnetN)        { return fGigaTrackerMCBMagnetPosition[magnetN];      }
  TVector3 GetGigaTrackerMDXMagnetPosition()                         { return fGigaTrackerMDXMagnetPosition;               }
  TVector3 GetGigaTrackerScraperMagnetPosition()                     { return fGigaTrackerScraperMagnetPosition;           }
  TVector3 GetGigaTrackerCollimatorPosition()                        { return fGigaTrackerCollimatorPosition;              }

  //Support
  TVector3 GetGigaTrackerSupportPosition(const int station)          { return fGigaTrackerSupportPosition[station];        }

  //Sensors
  TVector3 GetGigaTrackerSensorAssemblyLength(const int station)     { return fGigaTrackerSensorAssemblyLength[station];   } 
  TVector3 GetGigaTrackerSensorPosition(const int station)           { return fGigaTrackerSensorPosition[station];         }

  //Bump bondings
  TVector3 GetGigaTrackerBumpBondingPosition(const int station)      { return fGigaTrackerBumpBondingPosition[station];    }
  
  //Chips
  TVector3 GetGigaTrackerChipPosition(const int station)             { return fGigaTrackerChipPosition[station];           }

  //Digitizer
  // double   GetSplineT1(double x);
  // double   GetSplineToT(double x);
  double   GetQFromEn(double energy);
  double   GetQCut()                                                 { return fQcut;                                       }
  double   GetQSaturation()                                          { return fQsaturation;                                }
  double   GetToTFix()                                               { return fToTFix;                                     }

  void Print();
};
#endif
