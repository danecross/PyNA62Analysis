// LAVDataCardMessenger.cc
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - added option to provide threshold table in input (to be completed) 
// - added management of Additional Slewing Correction
// - added doxygen compliant documentation
// Created by T. Spadaro and E. Leonardi (tommaso.spadaro@cern.ch, emanuele.leonardi@cern.ch) 2015-01-22
// --------------------------------------------------------------
/// \class LAVDataCardMessenger
/// \Brief
/// Class for the management of LAV configuration input
/// \EndBrief
/// \Detailed
#include "LAVDataCardMessenger.hh"
#include "LAVGeometry.hh"
#include "Riostream.h"
#include "TObjString.h"
#include "TRegexp.h"
#include "NA62Global.hh"

LAVDataCardMessenger* LAVDataCardMessenger::fInstance = 0;

LAVDataCardMessenger::LAVDataCardMessenger(){

  for(Int_t layer = 0; layer < 5; layer++) 
    for(Int_t i = 0; i < 12; i++) fStationsMCToF[layer][i] = 0.;

  fT0NominalValuesProvided = kFALSE;
  fT0FileNameProvided = kFALSE;

  fThresholdNominalValuesProvided = kFALSE;
  fThresholdFileNameProvided = kFALSE;

  fMakeLAVT0s = kFALSE;

// fLAVT0CorrectionInputMode = 1--> use hard-coded values 
// fLAVT0CorrectionInputMode = 2--> use station-by-station values from user input 
// fLAVT0CorrectionInputMode = 3--> use block-by-block values in the input file provided 
// fLAVT0CorrectionInputMode = 4--> retrieve values from db (to be implemented)

  fLAVT0CorrectionInputMode = 1; 
  fLAVT0CorrectionFileName = ""; // T0 filename
  for (Int_t i=0; i<12; i++) fLAVT0CorrectionValues[i] = 0; // initialized to no-value

  fLAVSlewingCorrections = 0; // 1 --> make residual slewing corrections to LAV Reconstructed hits
  fLAVMCSlewingCorrectionParametersProvided = 0; // 1 --> Provide slewing correction parameters for MC
  for (Int_t j=0; j<2; j++) 
    for (Int_t i=0; i<5; i++) 
      fMCSlewingPars[j][i] = 0.;

// fLAVThresholdInputMode = 1--> use hard-coded values 
// fLAVThresholdInputMode = 2--> use station-by-station values from user input 
// fLAVThresholdInputMode = 3--> use block-by-block values in the input file provided 
// fLAVThresholdInputMode = 4--> retrieve values from db (to be implemented)
// fLAVThresholdInputMode = 5--> retrieve values from hardcoded table

  fLAVThresholdInputMode = 1; // 1 --> read from Geom defaults; 2 --> read from datacard a fixed value; 3 --> read from file a block by block value; 4 --> read from db a block by block value
  fLAVThresholdFileName = ""; // threshold filename

  fLAVThresholdLowValue = -1; // initialized to no-value
  fLAVThresholdHighValue = -1; // initialized to no-value
  fLAVHysteresisValue = -1; // initialized to no-value

  fMakeLAVClusters = 0; // 1 --> make LAV Clusters
  fMakeLAVTracks = 0;   // 1 --> make LAV tracks

  fMakeMCHitHistos = 0; // 0 --> no histos; 1 --> make general histos; 2 --> make detailed histos 
  fMakeDigiHistos = 0; //  0 --> no histos; 1 --> make general histos; 2 --> make detailed histos 
  fMakeRecoHistos = 0; //  0 --> no histos; 1 --> make general histos; 2 --> make detailed histos 

  fPreferredTimeAlgorithm = 0;   // 0 --> prefer slope; 1 --> prefer tot_low; 2 --> prefer tot_high

}

LAVDataCardMessenger * LAVDataCardMessenger::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new LAVDataCardMessenger(); }
  return fInstance;
}

Double_t LAVDataCardMessenger::GetLAVT0CorrectionValue(Int_t i){
  if (i<0 || i>11) {
    std::cerr << "LAVDataCardMessenger >> GetLAVT0CorrectionValue wrong input " << i << std::endl;
    exit(kGenericError);
  }
  return fLAVT0CorrectionValues[i];
}

Double_t LAVDataCardMessenger::GetLAVMCToFLayer(Int_t iLayer,Int_t iStation){
  if (iLayer<0 || iLayer >4 || iStation < 0 || iStation > 11 || (iStation > 7 && iLayer == 4)) {
    std::cerr << "LAVDataCardMessenger >> GetLAVMCToFLayer wrong input Layer=" << iLayer << " Station="<<iStation << std::endl;
    exit(kWrongConfiguration);
  }
  return fStationsMCToF[iLayer][iStation];
}


Double_t LAVDataCardMessenger::GetLAVDataSlewingCorrectionParameter(Int_t i, Int_t j){
  if (i<0 || i>1 || j<0 || j>4 || (i==0 && j>3)) {
    std::cerr << "LAVDataCardMessenger >> GetLAVDataSlewingCorrectionParameter wrong input " << i << " " << j << std::endl;
    exit(kWrongConfiguration);
  }
  return fDataSlewingPars[i][j];
}

Double_t LAVDataCardMessenger::GetLAVMCSlewingCorrectionParameter(Int_t i, Int_t j){
  if (i<0 || i>1 || j<0 || j>4 || (i==0 && j>3)) {
    std::cerr << "LAVDataCardMessenger >> GetLAVMCSlewingCorrectionParameter wrong input " << i << " " << j << std::endl;
    exit(kWrongConfiguration);
  }
  return fMCSlewingPars[i][j];
}

// Parse the LAV config file
void LAVDataCardMessenger::ParseConfFile(TString LAVConfigFileName){

  std::ifstream confFile(LAVConfigFileName.Data());
  if(!confFile.is_open()) {
    perror(LAVConfigFileName);
    exit(kWrongConfiguration);
  }

  TString Line;
  while(Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;

    TObjArray * l = Line.Tokenize(" ");
    Int_t numberOfStrings = l->GetEntriesFast();

    TString action = ((TObjString*)(l->At(0)))->GetString();

    if (action.CompareTo("MakeChannelT0s") == 0) {
      fMakeLAVT0s = kTRUE;
    }
    else if (action.CompareTo("T0ReadMethod") == 0) { // Method for T0 retrieving
      if (numberOfStrings != 2) {
	std::cerr << "LAVDataCardMessenger -- T0ReadMethod >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }      
      fLAVT0CorrectionInputMode = ((TObjString*)(l->At(1)))->GetString().Atoi(); // T0 read method
      if (fLAVT0CorrectionInputMode < 1 || fLAVT0CorrectionInputMode > 4) {
	std::cerr << "LAVDataCardMessenger -- Error: unknown input method for T0 correction --" << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
    }
    else if (action.CompareTo("T0InputFileName") == 0) { // FileName for T0 input
      if (numberOfStrings != 2) {
	std::cerr << "LAVDataCardMessenger -- T0FileName >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }      
      fLAVT0CorrectionFileName = ((TObjString*)(l->At(1)))->GetString();
      fT0FileNameProvided = kTRUE;
    }
    else if (action.CompareTo("T0NominalValues") == 0) { 
      if (numberOfStrings != 13) {
	std::cerr << "LAVDataCardMessenger -- T0NominalValue >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      for(Int_t i=0;i<12;i++) fLAVT0CorrectionValues[i] = ((TObjString*)(l->At(1+i)))->GetString().Atof();	
      fT0NominalValuesProvided = kTRUE;
    }
    else if (action.CompareTo("ThresholdReadMethod") == 0) { // Method for Threshold retrieving
      if (numberOfStrings != 2) {
	std::cerr << "LAVDataCardMessenger -- ThresholdReadMethod >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      fLAVThresholdInputMode = ((TObjString*)(l->At(1)))->GetString().Atoi(); // Threshold read method
      if (fLAVThresholdInputMode < 1 || fLAVThresholdInputMode > 5) {
	std::cerr << "LAVDataCardMessenger -- Error wrong ThresholdReadMethod input --" << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
    }
    else if (action.CompareTo("ThresholdInputFileName") == 0) { 
      if (numberOfStrings != 2) {
	std::cerr << "LAVDataCardMessenger -- ThresholdInputFile >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      fLAVThresholdFileName = ((TObjString*)(l->At(1)))->GetString();		
      fThresholdFileNameProvided = kTRUE;
    }
    else if (action.CompareTo("ThresholdNominalValues") == 0) { 
      if (numberOfStrings != 4) {
	std::cerr << "LAVDataCardMessenger -- ThresholdNominalInput >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      fLAVThresholdLowValue = ((TObjString*)(l->At(1)))->GetString().Atof();	
      fLAVThresholdHighValue = ((TObjString*)(l->At(2)))->GetString().Atof();	
      fLAVHysteresisValue = ((TObjString*)(l->At(3)))->GetString().Atof();	
      fThresholdNominalValuesProvided = kTRUE;
    }
    
    else if (action.CompareTo("ApplyLAVSlewingCorrections") == 0) {
      if (numberOfStrings != 2) {
	std::cerr << "LAVDataCardMessenger -- ApplyLAVSlewingCorrections >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      fLAVSlewingCorrections = ((TObjString*)(l->At(1)))->GetString().Atoi();
    }
    else if (action.CompareTo("LAVDataSlewingCorrectionParameters") == 0) {
      if (numberOfStrings != 10) {
	std::cerr << "LAVDataCardMessenger -- LAVDataSlewingCorrections >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      for (Int_t i=0; i<4; i++) fDataSlewingPars[0][i] = ((TObjString*)(l->At(1+i)))->GetString().Atof();
      for (Int_t i=0; i<5; i++) fDataSlewingPars[1][i] = ((TObjString*)(l->At(5+i)))->GetString().Atof();
      fLAVDataSlewingCorrectionParametersProvided = 1;
    }
    else if (action.CompareTo("LAVMCSlewingCorrectionParameters") == 0) {
      if (numberOfStrings != 10) {
	std::cerr << "LAVDataCardMessenger -- LAVMCSlewingCorrections >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      for (Int_t i=0; i<4; i++) fMCSlewingPars[0][i] = ((TObjString*)(l->At(1+i)))->GetString().Atof();
      for (Int_t i=0; i<5; i++) fMCSlewingPars[1][i] = ((TObjString*)(l->At(5+i)))->GetString().Atof();
      fLAVMCSlewingCorrectionParametersProvided = 1;
    }

    else if (action.CompareTo("MakeLAVClusters") == 0) {
      if (numberOfStrings != 2) {
	std::cerr << "LAVDataCardMessenger -- MakeLAVClusters >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      fMakeLAVClusters = ((TObjString*)(l->At(1)))->GetString().Atoi();
    }
    else if (action.CompareTo("MakeLAVTracks") == 0) {
      if (numberOfStrings != 2) {
	std::cerr << "LAVDataCardMessenger -- MakeLAVTracks >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      fMakeLAVTracks = ((TObjString*)(l->At(1)))->GetString().Atoi();
    }
    else if (action.CompareTo("MakeDigiHistos") == 0) {
      if (numberOfStrings != 2) {
	std::cerr << "LAVDataCardMessenger -- MakeDigiHistos >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      fMakeDigiHistos = ((TObjString*)(l->At(1)))->GetString().Atoi();
    }
    else if (action.CompareTo("MakeRecoHistos") == 0) {
      if (numberOfStrings != 2) {
	std::cerr << "LAVDataCardMessenger -- MakeRecoHistos >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      fMakeRecoHistos = ((TObjString*)(l->At(1)))->GetString().Atoi();
    }
    else if (action.CompareTo("PreferredTimeAlgorithm") == 0) {
      if (numberOfStrings != 2) {
	std::cerr << "LAVDataCardMessenger -- PreferredTimeAlgorithm >> Wrong input string in LAV config file: " << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      fPreferredTimeAlgorithm = ((TObjString*)(l->At(1)))->GetString().Atoi();
    }
    else if (action.CompareTo("MCStationsToFLayer") == 0) {
      if(numberOfStrings != 14) {
        std::cerr << "LAVDataCardMessenger -- Wrong number of stations in Layer TOF: " << numberOfStrings-1  << std::endl;
	exit(kWrongConfiguration);
      }
      Int_t layer = ((TObjString*)(l->At(1)))->GetString().Atoi();
      for(Int_t i = 0; i < 12; i++) fStationsMCToF[layer][i] = ((TObjString*)(l->At(i+2)))->GetString().Atof();
    }
    delete l;
  }
  confFile.close();

// verify input consistency

  if (fLAVT0CorrectionInputMode == 2 && !fT0NominalValuesProvided) {
    std::cerr << "LAVDataCardMessenger inconsistency: missing T0 nominalvalue" << std::endl;
    exit(kWrongConfiguration);
  }
  if (fLAVThresholdInputMode == 2 && !fThresholdNominalValuesProvided) {
    std::cerr << "LAVDataCardMessenger inconsistency: missing threshold nominalvalues" << std::endl;
    exit(kWrongConfiguration);
  }
  if (fLAVThresholdInputMode == 3 && !fThresholdFileNameProvided) {
    std::cerr << "LAVDataCardMessenger inconsistency: missing Threshold FileName" << std::endl;
    exit(kWrongConfiguration);
  }
  //Print();
}

void LAVDataCardMessenger::Print(){
  std::cout << std::endl;
  std::cout << "LAVDataCardMessenger: T0 Configuration " << std::endl;
  std::cout << "T0 NominalValueProvided = " << fT0NominalValuesProvided << " T0FileNameProvided = " << fT0FileNameProvided << " MakeT0s " << fMakeLAVT0s << std::endl;
  if (fT0NominalValuesProvided) {for (Int_t i=0;i<12;i++) std::cout << "Station " << i+1 << " T0 = " << fLAVT0CorrectionValues[i] << std::endl;}
  std::cout << "T0Correction Mode = " << fLAVT0CorrectionInputMode << std::endl; // 1 --> read from file; 2 --> read from db
  if (fT0FileNameProvided) std::cout << " T0FileName " << fLAVT0CorrectionFileName.Data() << std::endl; // T0 filename
  std::cout << std::endl;

  std::cout << "LAVDataCardMessenger: Threshold Configuration " << std::endl;
  std::cout << "Threshold NominalValueProvided = " << fThresholdNominalValuesProvided << " ThresholdFileNameProvided = " << fThresholdFileNameProvided << std::endl;
  if (fThresholdNominalValuesProvided) {std::cout << "Low " << fLAVThresholdLowValue << " High = " << fLAVThresholdHighValue << " Hyst = " << fLAVHysteresisValue << std::endl;}
  std::cout << "Threshold Mode = " << fLAVThresholdInputMode << std::endl; // 1 --> read from file; 2 --> read from db
  if (fThresholdFileNameProvided) std::cout << " Threshold FileName " << fLAVThresholdFileName.Data() << std::endl; // Threshold filename
  std::cout << std::endl;

  std::cout << "fMakeLAVClusters " << fMakeLAVClusters << " fMakeLAVTracks " << fMakeLAVTracks << std::endl;
  std::cout << "fMakeMCHitHistos " << fMakeMCHitHistos << " fMakeDigiHistos " << fMakeDigiHistos << " fMakeRecoHistos "<< fMakeRecoHistos << std::endl;
}
