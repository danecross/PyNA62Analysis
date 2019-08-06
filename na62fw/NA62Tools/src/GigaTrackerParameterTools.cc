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
// Everything is set in mm
// --------------------------------------------------------------
//

#include "GigaTrackerParameterTools.hh"
#include "NA62ConditionsService.hh"
#include "NA62Global.hh"

#include "TObjArray.h"
#include "TVector3.h"
#include "TObjString.h"


GigaTrackerParameterTools* GigaTrackerParameterTools::fInstance = nullptr;

GigaTrackerParameterTools* GigaTrackerParameterTools::GetInstance(int runID, bool isMC){
  if(!fInstance) fInstance = new GigaTrackerParameterTools(runID, isMC);
  return fInstance;
}


GigaTrackerParameterTools::GigaTrackerParameterTools(int CurrentRun, bool isMC){

  fIsMC = isMC;

  fGigaTrackerStationPositionRaw[0].SetXYZ(0.0,   0.0,  79600);  // 79.580 in old GigaTrackerGeometry.cc
  fGigaTrackerStationPositionRaw[1].SetXYZ(0.0, -60.0,  92800);
  fGigaTrackerStationPositionRaw[2].SetXYZ(0.0,   0.0, 102400);  // 102.420 in old GigaTrackerGeometry.cc
  fGigaTrackerStationPositionRaw[3].SetXYZ(0.0,   0.0, 102400);  // GTK4
  
  fGigaTrackerStationLength[0].SetXYZ(435.6, 146.0, 30.0);  //0.160); // x,y: MC Vessel?,z value from Niels file
  fGigaTrackerStationLength[1].SetXYZ(435.6, 146.0, 30.0);  //0.160); // z in MC: 0.03, z in reco: 0.2, z taken from Niels file
  fGigaTrackerStationLength[2].SetXYZ(435.6, 146.0, 30.0);  //28);    // stationZLength from GigaTrackerReconstruction.cc: 200e-6*1.e3, z from Niels file
  fGigaTrackerStationLength[3].SetXYZ(435.6, 146.0, 30.0);

  fGigaTrackerMCBMagnetRawPosition[0].SetXYZ(0.0,   0.0, 82980.0); //4A//86.580 in old reco
  fGigaTrackerMCBMagnetRawPosition[1].SetXYZ(0.0, -50.0, 86580.0); //4B
  fGigaTrackerMCBMagnetRawPosition[2].SetXYZ(0.0, -50.0, 95860.0); //5 //96.180 in old reco
  fGigaTrackerMCBMagnetRawPosition[3].SetXYZ(0.0,   0.0, 99460.0); //6 //99.780 in old reco 

  fGigaTrackerChipLength[0].SetXYZ(12.0, 19.5, 100.0e-3);
  fGigaTrackerChipLength[1].SetXYZ(12.0, 19.5, 100.0e-3);
  fGigaTrackerChipLength[2].SetXYZ(12.0, 19.5, 100.0e-3);
  fGigaTrackerChipLength[3].SetXYZ(12.0, 19.5, 100.0e-3);

  fGigaTrackerSensorLength[0].SetXYZ(63.1, 29.3, 200.0e-3); // most likely without guard ring;
  fGigaTrackerSensorLength[1].SetXYZ(63.1, 29.3, 200.0e-3);
  fGigaTrackerSensorLength[2].SetXYZ(63.1, 29.3, 200.0e-3);
  fGigaTrackerSensorLength[3].SetXYZ(63.1, 29.3, 200.0e-3);

  fGigaTrackerActiveSensorLength[0].SetXYZ(60.8, 27.0, 200.0e-3); // most likely without guard ring;
  fGigaTrackerActiveSensorLength[1].SetXYZ(60.8, 27.0, 200.0e-3);
  fGigaTrackerActiveSensorLength[2].SetXYZ(60.8, 27.0, 200.0e-3);
  fGigaTrackerActiveSensorLength[3].SetXYZ(60.8, 27.0, 200.0e-3);

  fGigaTrackerSensorAddAssemblyLength[0].SetXYZ(0.0, 9.9, 0.0);
  fGigaTrackerSensorAddAssemblyLength[1].SetXYZ(0.0, 9.9, 0.0);
  fGigaTrackerSensorAddAssemblyLength[2].SetXYZ(0.0, 9.9, 0.0);
  fGigaTrackerSensorAddAssemblyLength[3].SetXYZ(0.0, 9.9, 0.0);

  fCoolingPlateLength[0].SetXYZ(80.0, 70.0, 0.74); //z raw length, where top has to be subtracted
  fCoolingPlateLength[1].SetXYZ(80.0, 70.0, 0.74);
  fCoolingPlateLength[2].SetXYZ(80.0, 70.0, 0.74);
  fCoolingPlateLength[3].SetXYZ(80.0, 70.0, 0.74);

  for(int i = 0 ; i<fNStationsMax;i++){
    fGigaTrackerStationPositionMC[i].SetXYZ(0.0,0.0,0.0);
    fGigaTrackerStationPositionCorr[i].SetXYZ(0.0,0.0,0.0);
    fGigaTrackerMCBMagnetPosition[i].SetXYZ(0.0,0.0,0.0);
    fGigaTrackerSupportPosition[i].SetXYZ(0.0,0.0,0.0);
    fGigaTrackerSensorAssemblyLength[i].SetXYZ(0.0,0.0,0.0);
    fGigaTrackerSensorPosition[i].SetXYZ(0.0,0.0,0.0);
    fGigaTrackerBumpBondingPosition[i].SetXYZ(0.0,0.0,0.0);
    fGigaTrackerChipPosition[i].SetXYZ(0.0,0.0,0.0);
    fGigaTrackerStationMisalignment[i].SetXYZ(0.0,0.0,0.0);
    fGigaTrackerStationOffset[i].SetXYZ(0.0,0.0,0.0);
  }

  fStationFile    = "GigaTracker-Stations.dat";
  fPCBParFile     = "GigaTracker-PCB.dat";
  fSensorLFile    = "GigaTracker-SensorLength.dat";  
  fCoolPlatesFile = "GigaTracker-CoolingPlates.dat";
  fCollimatorFile = "GigaTracker-Collimator.dat";
  fTZeroFile      = "GigaTracker-T0.dat";       // not yet used here
  fTWalkFile      = "GigaTracker-TimeWalk.dat"; // not yet used here
  fRotationFile   = "GigaTracker-Rotation.dat"; // not yet used here
  fOffsetFile     = "GigaTracker-XYOffset.dat";
  fXYFile         = (fIsMC) ? "GigaTracker-XYCorrectionsMC.dat" : "GigaTracker-XYCorrections.dat";

  //local variables
  TString Line;
  TString fileName = fStationFile;
  if(NA62ConditionsService::GetInstance()->Open(fileName)!=kSuccess){
    std::cout << "[GigaTrackerParameterTools] Run: "<<CurrentRun<<" Error: failed not open input file " <<fileName<< std::endl;
    exit(kGenericError);
  }
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fileName))){
    if(Line.BeginsWith("#")) continue;
    else if(Line.BeginsWith("StationsSetUp")){
      TObjArray *l  = Line.Tokenize(" ");
      bool station1 = ((TObjString*)(l->At(1)))->GetString().Atoi();
      bool station2 = ((TObjString*)(l->At(2)))->GetString().Atoi();
      bool station3 = ((TObjString*)(l->At(3)))->GetString().Atoi();
      bool station4 = ((TObjString*)(l->At(4)))->GetString().Atoi();
      delete l;
      fStationIn[0] = bool(station1);
      fStationIn[1] = bool(station2);
      fStationIn[2] = bool(station3);
      fStationIn[3] = bool(station4);
      fNStations    = int(fStationIn[0]) + int(fStationIn[1]) + int(fStationIn[2]) + int(fStationIn[3]);
    }
    else if(Line.BeginsWith("SensorUpstream")){
      TObjArray *l = Line.Tokenize(" ");
      int station1 = ((TObjString*)(l->At(1)))->GetString().Atoi();
      int station2 = ((TObjString*)(l->At(2)))->GetString().Atoi();
      int station3 = ((TObjString*)(l->At(3)))->GetString().Atoi();
      int station4 = ((TObjString*)(l->At(4)))->GetString().Atoi();
      delete l;
      fGTKSensorUpstream[0] = bool(station1);
      fGTKSensorUpstream[1] = bool(station2);
      fGTKSensorUpstream[2] = bool(station3);
      fGTKSensorUpstream[3] = bool(station4);
    }
  }
  NA62ConditionsService::GetInstance()->Close(fileName); 
  Line.Clear();
  
  //Misaligment
  fileName = fXYFile;
  if(NA62ConditionsService::GetInstance()->Open(fileName)!=kSuccess){
    std::cout << "[GigaTrackerParameterTools] Run: "<<CurrentRun<<" Error: failed not open input file " <<fileName<< std::endl;
    exit(kGenericError);
  }

  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fileName))){
    if(Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    int station  = ((TObjString*)(l->At(0)))->GetString().Atoi();
    double dx    = ((TObjString*)(l->At(1)))->GetString().Atof();
    double dy    = ((TObjString*)(l->At(2)))->GetString().Atof();
    delete l;
    fGigaTrackerStationMisalignment[station].SetXYZ(dx, dy, 0.0);
  }
  NA62ConditionsService::GetInstance()->Close(fileName);
  Line.Clear();


  //Offset
  fileName = fOffsetFile;
  if(NA62ConditionsService::GetInstance()->Open(fileName)!=kSuccess){
    std::cout << "[GigaTrackerParameterTools] Run: "<<CurrentRun<<" Error: failed not open input file " <<fileName<< std::endl;
    exit(kGenericError);
  }

  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fileName))){
    if(Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    int station  = ((TObjString*)(l->At(0)))->GetString().Atoi();
    double dx    = ((TObjString*)(l->At(1)))->GetString().Atof();
    double dy    = ((TObjString*)(l->At(2)))->GetString().Atof();
    delete l;
    fGigaTrackerStationOffset[station].SetXYZ(dx, dy, 0.0);
  }
  NA62ConditionsService::GetInstance()->Close(fileName);
  Line.Clear();


  //Collimator
  fileName = fCollimatorFile; 
  if(NA62ConditionsService::GetInstance()->Open(fileName)!=kSuccess){
    std::cout << "[GigaTrackerParameterTools] Run: "<<CurrentRun<<" Error: failed not open input file " <<fileName<< std::endl;
    exit(kGenericError);
  }
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fileName))){
    if(Line.BeginsWith("#")) continue;
    else if(Line.BeginsWith("Design")){
      TObjArray *l = Line.Tokenize(" ");
      fGigaTrackerCollimatorDesign = ((TObjString*)(l->At(1)))->GetString();
    }    
    else if(Line.BeginsWith("Outer")){
      TObjArray *l = Line.Tokenize(" ");
      double sx    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double sy    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double sz    = ((TObjString*)(l->At(3)))->GetString().Atof();
      delete l;
      fGigaTrackerCollimatorOuterLength.SetXYZ(sx,sy,sz);
    }
    else if(Line.BeginsWith("Inner")){
      TObjArray *l = Line.Tokenize(" ");
      double sx    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double sy    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double sz    = ((TObjString*)(l->At(3)))->GetString().Atof();
      delete l;
      fGigaTrackerCollimatorInnerLength.SetXYZ(sx,sy,sz);
    }
    else if(Line.BeginsWith("Position")){
      TObjArray *l = Line.Tokenize(" ");
      double sx    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double sy    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double sz    = ((TObjString*)(l->At(3)))->GetString().Atof();
      delete l;
      fGigaTrackerCollimatorRawPosition.SetXYZ(sx,sy,sz);
    }
    else if(Line.BeginsWith("GDML")){
      TObjArray *l = Line.Tokenize(" ");
      TString gdmlfile = ((TObjString*)(l->At(1)))->GetString();
      fGigaTrackerCollimatorGDML = NA62ConditionsService::GetInstance()->GetFullPath(gdmlfile);
    }
  }
  NA62ConditionsService::GetInstance()->Close(fileName); 
  Line.Clear();

  //PCB
  fileName = fPCBParFile;
  if(NA62ConditionsService::GetInstance()->Open(fileName)!=kSuccess){
    std::cout << "[GigaTrackerParameterTools] Run: "<<CurrentRun<<" Error: failed not open input file " <<fileName<< std::endl;
    exit(kGenericError);
  }
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fileName))){
    if(Line.BeginsWith("#")) continue;
    else if(Line.BeginsWith("Length")){
      TObjArray *l = Line.Tokenize(" ");
      double sx    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double sy    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double sz    = ((TObjString*)(l->At(3)))->GetString().Atof();
      delete l; 
      fGigaTrackerPCBLength.SetXYZ(sx,sy,sz);
    }
    else if(Line.BeginsWith("Hole")){
      TObjArray *l = Line.Tokenize(" ");
      double sx    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double sy    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double sz    = ((TObjString*)(l->At(3)))->GetString().Atof();
      delete l;
      fGigaTrackerPCBHoleLength.SetXYZ(sx,sy,sz);
    }
    else if(Line.BeginsWith("Offset")){
      TObjArray *l = Line.Tokenize(" ");
      double sx    = ((TObjString*)(l->At(1)))->GetString().Atof();
      delete l;
      fGigaTrackerPCBHoleXOffset = sx;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fileName); 
  Line.Clear();

  //Sensors
  fileName = fSensorLFile;
  if(NA62ConditionsService::GetInstance()->Open(fileName)!=kSuccess){
    std::cout << "[GigaTrackerParameterTools] Run: "<<CurrentRun<<" Error: failed not open input file " <<fileName<< std::endl;
    exit(kGenericError);
  }
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fileName))){
    if(Line.BeginsWith("#")) continue;
    else if(Line.BeginsWith("Length")){
      TObjArray *l = Line.Tokenize(" ");
      int station  = ((TObjString*)(l->At(1)))->GetString().Atoi();
      double sx    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double sy    = ((TObjString*)(l->At(3)))->GetString().Atof();
      double sz    = ((TObjString*)(l->At(4)))->GetString().Atof();
      delete l;
      fGigaTrackerSensorLength[station].SetXYZ(sx,sy,sz);    
    }
    else if(Line.BeginsWith("Active")){
      TObjArray *l = Line.Tokenize(" ");
      int station  = ((TObjString*)(l->At(1)))->GetString().Atoi();
      double sx    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double sy    = ((TObjString*)(l->At(3)))->GetString().Atof();
      double sz    = ((TObjString*)(l->At(4)))->GetString().Atof();
      delete l;
      fGigaTrackerActiveSensorLength[station].SetXYZ(sx, sy, sz);
    }
    else if(Line.BeginsWith("Assembly")){
      TObjArray *l = Line.Tokenize(" ");
      int station  = ((TObjString*)(l->At(1)))->GetString().Atoi();
      double dx    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double dy    = ((TObjString*)(l->At(3)))->GetString().Atof();
      double dz    = ((TObjString*)(l->At(4)))->GetString().Atof();
      delete l;
      fGigaTrackerSensorAddAssemblyLength[station].SetXYZ(dx, dy, dz);
    }
    else if(Line.BeginsWith("Bump")){
      TObjArray *l = Line.Tokenize(" ");
      double dr    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double dz    = ((TObjString*)(l->At(2)))->GetString().Atof();
      delete l;
      fGigaTrackerBumpBondingRLength = dr;
      fGigaTrackerBumpBondingZLength = dz;
    }

  }
  NA62ConditionsService::GetInstance()->Close(fileName); 
  Line.Clear();

  //Cooling plates
  fileName = fCoolPlatesFile;
  if(NA62ConditionsService::GetInstance()->Open(fileName)!=kSuccess){
    std::cout << "[GigaTrackerParameterTools] Run: "<<CurrentRun<<" Error: failed not open input file " <<fileName<< std::endl;
    exit(kGenericError);
  }
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fileName))){
    if(Line.BeginsWith("#")) continue;
    else if(Line.BeginsWith("Length")){
      TObjArray *l = Line.Tokenize(" ");
      int station  = ((TObjString*)(l->At(1)))->GetString().Atoi();
      double dx    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double dy    = ((TObjString*)(l->At(3)))->GetString().Atof();
      double dz    = ((TObjString*)(l->At(4)))->GetString().Atof();
      delete l;
      fCoolingPlateLength[station].SetXYZ(dx, dy, dz);
    }
    else if(Line.BeginsWith("TopLength")){
      TObjArray *l = Line.Tokenize(" ");
      double xS    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double xH    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double yS    = ((TObjString*)(l->At(3)))->GetString().Atof();
      double yH    = ((TObjString*)(l->At(4)))->GetString().Atof();  
      delete l;
      
      fCoolingPlateTopShoulderLength.SetXYZ(xS,yS,0.0);
      fCoolingPlateTopHollowLength.SetXYZ(xH,yH,0.0); 
    }
    else if(Line.BeginsWith("TopDepth")){
      TObjArray *l = Line.Tokenize(" ");
      double d1    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double d2    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double d3    = ((TObjString*)(l->At(3)))->GetString().Atof();
      double d4    = (fNStations > 3) ? ((TObjString*)(l->At(4)))->GetString().Atof() : 0.0;
      delete l;

      fCoolingPlateTopDepth[0] = d1; 
      fCoolingPlateTopDepth[1] = d2; 
      fCoolingPlateTopDepth[2] = d3; 
      fCoolingPlateTopDepth[3] = d4; 
    }
    else if(Line.BeginsWith("BottomLength")){
      TObjArray *l = Line.Tokenize(" ");
      double xS    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double xH    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double yS    = ((TObjString*)(l->At(3)))->GetString().Atof();
      double yH    = ((TObjString*)(l->At(4)))->GetString().Atof();  
      delete l;
      
      fCoolingPlateBottomShoulderLength.SetXYZ(xS,yS,0.0);
      fCoolingPlateBottomHollowLength.SetXYZ(xH,yH,0.0); 
    }

    else if(Line.BeginsWith("BottomDepth")){
      TObjArray *l = Line.Tokenize(" ");
      double d1    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double d2    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double d3    = ((TObjString*)(l->At(3)))->GetString().Atof();
      double d4    = (fNStations > 3) ? ((TObjString*)(l->At(4)))->GetString().Atof() : 0.0;
      delete l;
      fCoolingPlateBottomDepth[0] = d1; 
      fCoolingPlateBottomDepth[1] = d2; 
      fCoolingPlateBottomDepth[2] = d3; 
      fCoolingPlateBottomDepth[3] = d4; 
    }

    else if(Line.BeginsWith("Envelope")){
      TObjArray *l = Line.Tokenize(" ");
      double sx    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double sy    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double sz    = ((TObjString*)(l->At(3)))->GetString().Atof();
      delete l;
      fCoolingPlateChannelsEnvelopeLength.SetXYZ(sx,sy,sz);    
    }
    else if(Line.BeginsWith("Glue")){
      TObjArray *l = Line.Tokenize(" ");
      double gl    = ((TObjString*)(l->At(1)))->GetString().Atof();
      delete l;
      fGigaTrackerGlueLayerZLength = gl;
    }
    else if(Line.BeginsWith("Channel")){
      TObjArray *l = Line.Tokenize(" ");
      double xo    = ((TObjString*)(l->At(1)))->GetString().Atof();
      double de    = ((TObjString*)(l->At(2)))->GetString().Atof();
      double cx    = ((TObjString*)(l->At(3)))->GetString().Atof();
      double hwx   = ((TObjString*)(l->At(4)))->GetString().Atof();
      delete l;
      fCoolingPlateChannelsXOffset        = xo;
      fCoolingPlateChannelsDepth          = de;
      fCoolingPlateChannelCoolantXLength  = cx;
      fCoolingPlateChannelHalfWallXLength = hwx;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fileName);
  Line.Clear();

  //--------------- Calculate from parameters set in .hh or before here ---------------//
  SetGigaTrackerDetectorZPosition();
  SetGigaTrackerStationPositionMC();        // for MC, add offset from input file
  SetGigaTrackerStationPositionCorrected(); // add misalignment from input file
  SetGigaTrackerResponseRegion();
    
  // Magnets
  // MCB
  if(fNStations == 4) fGigaTrackerMCBMagnetRawPosition[0].SetXYZ(0.0,0.0,fMCBGTK4PostionZ); // for GTK4    
  SetGigaTrackerMCBMagnetsPosition();
  // MDX
  SetGigaTrackerMDXMagnetPosition();
  // Scraper
  SetGigaTrackerScraperMagnetPosition();
  // Collimator
  SetGigaTrackerCollimatorPosition();
  
  // Support --NOT used!
  // SetGigaTrackerSupportPosition();

  // Sensors
  SetGigaTrackerSensorAssemblyLength(); 
  SetGigaTrackerSensorPosition();

  // BumpBondings
  SetGigaTrackerBumpBondingPosition(); 

  // Chips
  SetGigaTrackerChipPosition();
}

//__________________________________________________________________________________________________
//Stations corrected for z position and misalignment in MC
void GigaTrackerParameterTools::SetGigaTrackerStationPositionMC(){
  for(int i=0;i<fNStations;i++){
    double offset = 0.0;
    offset =  0.5*fGigaTrackerPCBLength.Z()  + fCoolingPlateTopDepth[i] - 
      0.5*fCoolingPlateLength[i].Z() - fGigaTrackerGlueLayerZLength - fGigaTrackerChipLength[i].Z() -
      fGigaTrackerBumpBondingZLength - 0.5*fGigaTrackerSensorLength[i].Z();  
    
    fGigaTrackerStationPositionMC[i].SetXYZ(0.,0.,offset - fGigaTrackerDetectorZPosition);
    fGigaTrackerStationPositionMC[i] += fGigaTrackerStationOffset[i];
    fGigaTrackerStationPositionMC[i] += fGigaTrackerStationPositionRaw[i];
  }
}
//Stations corrected for z position
void GigaTrackerParameterTools::SetGigaTrackerStationPositionCorrected(){
  for(int i=0;i<fNStations;i++){
    fGigaTrackerStationPositionCorr[i].SetXYZ(0.,0., 0.5*fGigaTrackerSensorLength[i].Z() - fGigaTrackerDetectorZPosition);
    fGigaTrackerStationPositionCorr[i] += fGigaTrackerStationPositionRaw[i];
  }
}
//z position
void GigaTrackerParameterTools::SetGigaTrackerDetectorZPosition(){
  fGigaTrackerDetectorZPosition = 0.5*(fGigaTrackerStation1VXBOEndPosition + fGigaTrackerStation3BeforeCHANTIPosition); 
  //fGigaTrackerStationPositionRaw[0].Z() + //or? check!!!
  //				       (fGigaTrackerStationPositionRaw[2].Z() + 0.5*fGigaTrackerStationLength[2].Z()));//0.5 * (79.440 =VXBO + 102.420);//*m; in MC
  // in old reco 0.5 * (79.580 + 102.420) which is 20 mm upstream of GTK1 position in beatch file
  // 102.420 is fGigaTrackerStation3BeforeCHANTIPosition which is 20 mm downstream of GTK3
}
//response region
void GigaTrackerParameterTools::SetGigaTrackerResponseRegion(){
  fGigaTrackerResponseRegion.SetXYZ(4000.0, 4000.0, fGigaTrackerStation3BeforeCHANTIPosition - 
				    fGigaTrackerStation1VXBOEndPosition);
  //fGigaTrackerStationPositionRaw[2].Z() + 0.5*fGigaTrackerStationLength[2].Z()) //or VXBO???
  //fGigaTrackerStationPositionRaw[0].Z());//check meaning and value!       
}
//__________________________________________________________________________________________________
//Magnets
void GigaTrackerParameterTools::SetGigaTrackerMCBMagnetsPosition(){
  for(int i=0;i<4;i++){
    fGigaTrackerMCBMagnetPosition[i].SetXYZ(fGigaTrackerMCBMagnetRawPosition[i].X(),fGigaTrackerMCBMagnetRawPosition[i].Y(),
                                            fGigaTrackerMCBMagnetRawPosition[i].Z() - 0.5 * fGigaTrackerMCBMagnetLength.Z() - 
					    fGigaTrackerDetectorZPosition);
  }
}
void GigaTrackerParameterTools::SetGigaTrackerMDXMagnetPosition(){
  fGigaTrackerMDXMagnetPosition.SetXYZ(fGigaTrackerMDXMagnetRawPosition.X(),fGigaTrackerMDXMagnetRawPosition.Y(),
                                       fGigaTrackerMDXMagnetRawPosition.Z() - 0.5 * fGigaTrackerMDXMagnetLength.Z() - 
				       fGigaTrackerDetectorZPosition);
}

void GigaTrackerParameterTools::SetGigaTrackerScraperMagnetPosition(){
  fGigaTrackerScraperMagnetPosition.SetXYZ(fGigaTrackerScraperMagnetRawPosition.X(),fGigaTrackerScraperMagnetRawPosition.Y(),
                                           fGigaTrackerScraperMagnetRawPosition.Z() - 0.5 * fGigaTrackerScraperMagnetLength.Z() - 
                                           fGigaTrackerDetectorZPosition);
}

void GigaTrackerParameterTools::SetGigaTrackerCollimatorPosition(){
  fGigaTrackerCollimatorPosition.SetXYZ(fGigaTrackerCollimatorRawPosition.X(),fGigaTrackerCollimatorRawPosition.Y(),
                                        fGigaTrackerCollimatorRawPosition.Z() - 0.5 * fGigaTrackerCollimatorOuterLength.Z() - 
                                        fGigaTrackerDetectorZPosition);
}
//__________________________________________________________________________________________________
void GigaTrackerParameterTools::SetGigaTrackerSensorAssemblyLength(){
  for(int i=0;i<fNStations;i++){
    double assemblyXLength =0.0,assemblyYLength =0.0,assemblyZLength =0.0;
    assemblyXLength = fGigaTrackerSensorLength[i].X() + fGigaTrackerSensorAddAssemblyLength[i].X();
    assemblyYLength = fGigaTrackerSensorLength[i].Y() + fGigaTrackerSensorAddAssemblyLength[i].Y(); //+ Chips!!! CHECK!!!!
    assemblyZLength = fGigaTrackerSensorLength[i].Z() + fGigaTrackerBumpBondingZLength + 
      fGigaTrackerChipLength[i].Z()   + fGigaTrackerGlueLayerZLength;
    fGigaTrackerSensorAssemblyLength[i].SetXYZ(assemblyXLength,assemblyYLength,assemblyZLength);
  }
}

void GigaTrackerParameterTools::SetGigaTrackerSensorPosition(){
  for(int i=0;i<fNStations;i++){
    fGigaTrackerSensorPosition[i].SetXYZ(0., 0., 0.5 * (fGigaTrackerSensorLength[i].Z()));
    // - fGigaTrackerSmallPixelLength.Z()));//check!!! w/o fGigaTrackerSmallPixelLength.Z() finally used in MC!
  }
}
//__________________________________________________________________________________________________
//BumpBondings
void GigaTrackerParameterTools::SetGigaTrackerBumpBondingPosition(){
  for(int i=0;i<fNStations;i++){
    fGigaTrackerBumpBondingPosition[i].SetXYZ(fGigaTrackerBumpBondingOffset.X(), fGigaTrackerBumpBondingOffset.Y(),
					      fGigaTrackerSensorLength[i].Z() + 0.5 * (fGigaTrackerBumpBondingZLength));
  } 
}
//__________________________________________________________________________________________________
//Chips
void GigaTrackerParameterTools::SetGigaTrackerChipPosition(){
  for(int i=0;i<fNStations;i++){
    fGigaTrackerChipPosition[i].SetXYZ(0., 0., 0.5 * (fGigaTrackerChipLength[i].Z()) + 
				       fGigaTrackerSensorLength[i].Z() + 
				       fGigaTrackerBumpBondingZLength);
  }
}
//__________________________________________________________________________________________________
//Splines used in Digitizer
// double GigaTrackerParameterTools::GetSplineT1(double x){
// const int Np = 27, Kstep = 0;
// const double Delta = -1, Xmin = 0.7, Xmax = 20;
// const double X[27] = { 0.7, 0.8, 0.9, 1, 1.2,
// 		 1.4, 1.6, 1.8, 2, 3,
// 		 4, 5, 6, 7, 8,
// 		 9, 10, 11, 12, 13,
// 		 14, 15, 16, 17, 18,
// 		 19, 20 };
// const double Y[27] = { 24.0011, 23.2665, 22.8532, 22.5687, 22.2009,
// 		 21.9278, 21.698, 21.5384, 21.4091, 21.1243,
// 		 20.8684, 20.7064, 20.6609, 20.5665, 20.4747,
// 		 20.4098, 20.3567, 20.3274, 20.2898, 20.2576,
// 		 20.2322, 20.1899, 20.1714, 20.1574, 20.1498,
// 		 20.1485, 20.163 };
// const double B[27] = { -9.71441, -5.35854, -3.28841, -2.4218, -1.47935,
// 		 -1.27429, -0.966977, -0.698799, -0.571327, -0.201982,
// 		 -0.242847, -0.0803304, -0.0583316, -0.106043, -0.0760948,
// 		 -0.0596774, -0.0391958, -0.0307395, -0.0385461, -0.0244762,
// 		 -0.0363492, -0.0332269, -0.0131433, -0.0116999, -0.00485718,
// 		 0.00442859, 0.0267428 };
// const double C[27] = { 27.4937, 16.065, 4.6363, 4.02979, 0.682473,
// 		 0.342815, 1.19376, 0.147125, 0.490235, -0.12089,
// 		 0.0800244, 0.0824923, -0.0604935, 0.0127816, 0.017167,
// 		 -0.000749516, 0.0212311, -0.0127749, 0.00496832, 0.00910158,
// 		 -0.0209747, 0.024097, -0.00401349, 0.00545693, 0.00138577,
// 		 0.0079, 1 };
// const double D[27] = { -38.0957, -38.0957, -2.0217, -5.57886, -0.566096,
// 		 1.41825, -1.7444, 0.571851, -0.203708, 0.0669715,
// 		 0.000822613, -0.0476619, 0.024425, 0.00146178, -0.00597216,
// 		 0.00732687, -0.0113353, 0.00591439, 0.00137776, -0.0100254,
// 		 0.0150239, -0.00937018, 0.00315681, -0.00135705, 0.00217141,
// 		 0.00217141, 0.464102 };
// int klow=0;
// if(x<=Xmin) klow=0;
// else if(x>=Xmax) klow=Np-1;
// else {
//   if(Kstep) {
//     // Equidistant knots, use histogramming
//     klow = int((x-Xmin)/Delta);
//     if (klow < Np-1) klow = Np-1;
//   } else {
//     int khig=Np-1, khalf;
//     // Non equidistant knots, binary search
//     while(khig-klow>1)
// if(x>X[khalf=(klow+khig)/2]) klow=khalf;
// else khig=khalf;
//   }
// }
// // Evaluate now
// double dx=x-X[klow];
// return (Y[klow]+dx*(B[klow]+dx*(C[klow]+dx*D[klow])));
// }

// double GigaTrackerParameterTools::GetSplineToT(double x) {
// const int Np = 27, Kstep = 0;
// const double Delta = -1, Xmin = 0.7, Xmax = 20;
// const double X[27] = { 0.7, 0.8, 0.9, 1, 1.2,
// 		 1.4, 1.6, 1.8, 2, 3,
// 		 4, 5, 6, 7, 8,
// 		 9, 10, 11, 12, 13,
// 		 14, 15, 16, 17, 18,
// 		 19, 20 };
// const double Y[27] = { 7.4883, 9.08846, 10.1765, 10.9989, 12.1626,
// 		 13.0018, 13.6094, 14.0966, 14.4967, 15.7248,
// 		 16.4219, 16.9265, 17.3294, 17.7192, 18.1361,
// 		 18.5425, 18.9598, 19.4065, 19.8306, 20.2526,
// 		 20.6691, 21.1292, 21.607, 22.1446, 22.4261,
// 		 22.5002, 22.5097 };
// const double B[27] = { 19.5206, 12.9618, 9.27824, 7.23847, 4.81219,
// 		 3.55626, 2.66475, 2.20673, 1.81781, 0.844388,
// 		 0.580237, 0.439763, 0.383211, 0.405493, 0.414916,
// 		 0.404743, 0.437211, 0.438411, 0.421544, 0.413711,
// 		 0.439111, 0.459645, 0.53601, 0.442517, 0.151224,
// 		 0.0193881, 0.0220238 };
// const double C[27] = { -39.9825, -25.606, -11.2295, -9.16818, -2.96323,
// 		 -3.3164, -1.14117, -1.1489, -0.795711, -0.177713,
// 		 -0.0864376, -0.0540368, -0.00251518, 0.0247975, -0.0153749,
// 		 0.00520222, 0.0272661, -0.0260665, 0.00919994, -0.0170333,
// 		 0.0424331, -0.0218993, 0.0982641, -0.191757, -0.0995357,
// 		 -0.0323, 1 };
// const double D[27] = { 47.9218, 47.9218, 6.87091, 10.3416, -0.588615,
// 		 3.62537, -0.0128822, 0.588654, 0.205999, 0.0304251,
// 		 0.0108003, 0.0171739, 0.00910424, -0.0133908, 0.00685905,
// 		 0.00735462, -0.0177775, 0.0117555, -0.00874441, 0.0198221,
// 		 -0.0214442, 0.0400545, -0.0966737, 0.0307405, 0.0224119,
// 		 0.0224119, 0.464102 };
// int klow=0;
// if(x<=Xmin) klow=0;
// else if(x>=Xmax) klow=Np-1;
// else {
//   if(Kstep) {
//     // Equidistant knots, use histogramming
//     klow = int((x-Xmin)/Delta);
//     if (klow < Np-1) klow = Np-1;
//   } else {
//     int khig=Np-1, khalf;
//     // Non equidistant knots, binary search
//     while(khig-klow>1)
// if(x>X[khalf=(klow+khig)/2]) klow=khalf;
// else khig=khalf;
//   }
// }
// // Evaluate now
// double dx=x-X[klow];
// return (Y[klow]+dx*(B[klow]+dx*(C[klow]+dx*D[klow])));
// }

double GigaTrackerParameterTools::GetQFromEn(double energy){
  double Q  = (energy * 100. * 1.6) / 3.6 ; 
  return Q;
}

void GigaTrackerParameterTools::Print(){

  std::cout << "[GigaTrackerParameterTools] : station included = " 
	    << fStationIn[0] << " " << fStationIn[1] << " " 
	    << fStationIn[2] << " " << fStationIn[3] << std::endl;
  
  std::cout << "[GigaTrackerParameterTools] : station sensor upstream = " 
	    << fGTKSensorUpstream[0] << " " << fGTKSensorUpstream[1] << " "
	    << fGTKSensorUpstream[2] << " " << fGTKSensorUpstream[3] << std::endl;
  
  std::cout << "[GigaTrackerParameterTools] : station misalingment [mm] = " << std::endl;
  for(int i = 0 ; i<fNStations;i++){
    std::cout << "[GigaTrackerParameterTools] " << "Station " << i << " " 
	      << fGigaTrackerStationMisalignment[i].X() << " " 
	      << fGigaTrackerStationMisalignment[i].Y() << " " 
	      << fGigaTrackerStationMisalignment[i].Z() << std::endl;
  }
  
  std::cout << "[GigaTrackerParameterTools] : station offsets [mm] = " << std::endl;
  for(int i = 0 ; i<fNStations;i++){
    std::cout << "[GigaTrackerParameterTools] " << "Station " << i << " " 
	      << fGigaTrackerStationOffset[i].X() << " " 
	      << fGigaTrackerStationOffset[i].Y() << " " 
	      << fGigaTrackerStationOffset[i].Z() << std::endl;
  }
  
  std::cout << "[GigaTrackerParameterTools] : collimator design: "<<fGigaTrackerCollimatorDesign<<std::endl;
  
  std::cout << "[GigaTrackerParameterTools] : collimator outer lenghts [mm]= " 
	    << fGigaTrackerCollimatorOuterLength.X() << " " 
	    << fGigaTrackerCollimatorOuterLength.Y() << " " 
	    << fGigaTrackerCollimatorOuterLength.Z() << std::endl;
  
  std::cout << "[GigaTrackerParameterTools] : collimator inner lengths [mm] = " 
	    << fGigaTrackerCollimatorInnerLength.X() << " " 
	    << fGigaTrackerCollimatorInnerLength.Y() << " "
	    << fGigaTrackerCollimatorInnerLength.Z() << std::endl;
  
  std::cout << "[GigaTrackerParameterTools] : collimator postition [mm]= " 
	    << fGigaTrackerCollimatorRawPosition.X() << " " 
	    << fGigaTrackerCollimatorRawPosition.Y() << " "
	    << fGigaTrackerCollimatorRawPosition.Z() << std::endl;
  
  std::cout << "[GigaTrackerParameterTools] : collimator GDML file path found "<<fGigaTrackerCollimatorGDML<<std::endl;
  
  std::cout << "[GigaTrackerParameterTools] : station PCB length [mm]= " 
	    << fGigaTrackerPCBLength.X() << " " 
	    << fGigaTrackerPCBLength.Y() << " " 
	    << fGigaTrackerPCBLength.Z() << std::endl;
  
  std::cout << "[GigaTrackerParameterTools] : station PCB hole length [mm]= " 
	    << fGigaTrackerPCBHoleLength.X() << " " 
	    << fGigaTrackerPCBHoleLength.Y() << " "
	    << fGigaTrackerPCBHoleLength.Z() << std::endl;
  
  std::cout << "[GigaTrackerParameterTools] : station PCB hole x offset [mm]= " 
	    << fGigaTrackerPCBHoleXOffset << std::endl;
  
  for(int i = 0 ; i<fNStations;i++){	           
    std::cout << "[GigaTrackerParameterTools] : station sensor length [mm]= " 
	      << fGigaTrackerSensorLength[i].X() << " " 
	      << fGigaTrackerSensorLength[i].Y() << " "
	      << fGigaTrackerSensorLength[i].Z() << std::endl;
  }
  
  std::cout << "[GigaTrackerParameterTools] : add to sensor for active lengths [mm]= " <<std::endl;
  for(int i = 0 ; i<fNStations;i++){
    std::cout << "[GigaTrackerParameterTools] " << "Station " << i << " " 
	      << fGigaTrackerActiveSensorLength[i].X() << " " 
	      << fGigaTrackerActiveSensorLength[i].Y() << " " 
	      << fGigaTrackerActiveSensorLength[i].Z() << std::endl;
  }
    
  std::cout << "[GigaTrackerParameterTools] : bump bonding r,z lengths [mm]= " 
	    << fGigaTrackerBumpBondingRLength << " " 
	    << fGigaTrackerBumpBondingZLength << std::endl;
    
  std::cout << "[GigaTrackerParameterTools] : cooling plate lengths [mm] = " <<std::endl;
  for(int i = 0.0;i<fNStations;i++){
    std::cout << "[GigaTrackerParameterTools] " << "Station " << i << " " 
	      << fCoolingPlateLength[i].X() << " " 
	      << fCoolingPlateLength[i].Y() << " "
	      << fCoolingPlateLength[i].Z() << std::endl;
  }
    
  std::cout << "[GigaTrackerParameterTools] : station cooling plate top info " << std::endl; 
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateTopShoulderXLength = " << fCoolingPlateTopShoulderLength.X() << std::endl;
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateTopHollowXLength   = " << fCoolingPlateTopHollowLength.X()   << std::endl; 
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateTopShoulderYLength = " << fCoolingPlateTopShoulderLength.Y() << std::endl; 
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateTopHollowYLength   = " << fCoolingPlateTopHollowLength.Y()   << std::endl; 
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateTopDepth[0]        = " << fCoolingPlateTopDepth[0]           << std::endl; 
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateTopDepth[1]        = " << fCoolingPlateTopDepth[1]           << std::endl; 
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateTopDepth[2]        = " << fCoolingPlateTopDepth[2]           << std::endl;
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateTopDepth[3]        = " << fCoolingPlateTopDepth[3]           << std::endl;

  std::cout << "[GigaTrackerParameterTools] : cooling plate bottom info  " << std::endl; 
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateBottomShoulderXLength = " << fCoolingPlateBottomShoulderLength.X() << std::endl; 
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateBottomHollowXLength   = " << fCoolingPlateBottomHollowLength.X()   << std::endl; 
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateBottomShoulderYLength = " << fCoolingPlateBottomShoulderLength.Y() << std::endl; 
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateBottomHollowYLength   = " << fCoolingPlateBottomHollowLength.Y()   << std::endl;
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateBottomDepth[0]        = " << fCoolingPlateBottomDepth[0]           << std::endl;
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateBottomDepth[1]        = " << fCoolingPlateBottomDepth[1]           << std::endl;
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateBottomDepth[2]        = " << fCoolingPlateBottomDepth[2]           << std::endl;
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateBottomDepth[3]        = " << fCoolingPlateBottomDepth[3]           << std::endl;

  std::cout << "[GigaTrackerParameterTools] : station sensor envelope length = " 
	    << fCoolingPlateChannelsEnvelopeLength.X() << " " 
	    << fCoolingPlateChannelsEnvelopeLength.Y() << " "
	    << fCoolingPlateChannelsEnvelopeLength.Z() << std::endl;
    
  std::cout << "[GigaTrackerParameterTools] : station glue layer = " << fGigaTrackerGlueLayerZLength << std::endl;
    
  std::cout << "[GigaTrackerParameterTools] : station channel parameters " << std::endl;
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateChannelsXOffset        = " << fCoolingPlateChannelsXOffset        << std::endl;
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateChannelsDepth          = " << fCoolingPlateChannelsDepth          << std::endl;
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateChannelCoolantXLength  = " << fCoolingPlateChannelCoolantXLength  << std::endl;
  std::cout << "[GigaTrackerParameterTools] " << "fCoolingPlateChannelHalfWallXLength = " << fCoolingPlateChannelHalfWallXLength << std::endl; 
}
