// LAVConfiguration.cc
// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - added option to provide threshold table in input (to be completed) 
// - added doxygen compliant documentation
// Created by T. Spadaro and E. Leonardi (tommaso.spadaro@cern.ch, emanuele.leonardi@cern.ch) 2015-01-22
// --------------------------------------------------------------
/// \class LAVConfiguration
/// \Brief
/// Class for the management of threshold and hysteresis settings.
/// \EndBrief
/// \Detailed
/// The ThresholdMethod is retrieved by the DataCardMessenger. Values have the following meaning:
/// ThresholdMethod = 1--> use hard-coded values 
/// ThresholdMethod = 2--> use station-by-station values from user input 
/// ThresholdMethod = 3--> use block-by-block values in the input file provided 
/// ThresholdMethod = 4--> retrieve values from db (to be implemented)
/// ThresholdMethod = 5--> retrieve values from hardcoded table (to be implemented)
/// \EndDetailed


#include "Riostream.h"
#include "TString.h"
#include "TObjString.h"
#include "TRegexp.h"
#include "LAVConfiguration.hh"
#include "LAVDataCardMessenger.hh"
#include "NA62ConditionsService.hh"
#include "NA62Global.hh"

LAVConfiguration* LAVConfiguration::fInstance = 0;

LAVConfiguration::LAVConfiguration(){

  for (Int_t i=0; i<MAXBLOCKMAP; i++) {
    fHighThreshold[i] = -1;
    fLowThreshold[i] = -1;
    fHysteresis[i] = -1;
  }
  fReadConfigurationInfos = kFALSE;
  fGeom = LAVGeometry::GetInstance();

  LAVDataCardMessenger* DataCard = LAVDataCardMessenger::GetInstance();
  Int_t thresholdMethod = DataCard->GetLAVThresholdInputMode();

  Double_t NominalThresholdHigh = -1;
  Double_t NominalThresholdLow = -1;
  Double_t NominalHysteresis = -1;

  if (thresholdMethod == 1) { // Take defaults from LAVGeometry

    for (Int_t i=0; i<MAXBLOCKMAP; i++) {
      fHighThreshold[i] = fGeom->GetLeadingThresholdHigh();
      fLowThreshold[i] = fGeom->GetLeadingThresholdLow();
      fHysteresis[i] = fGeom->GetHysteresiLow();
    }
    fReadConfigurationInfos = kTRUE;
  }
  else if (thresholdMethod == 2) { // Take defaults from DataCard
    for (Int_t i=0; i<MAXBLOCKMAP; i++) {
      fHighThreshold[i] = DataCard->GetLAVThresholdHighValue();
      fLowThreshold[i] = DataCard->GetLAVThresholdLowValue();
      fHysteresis[i] = DataCard->GetLAVHysteresisValue();
    }
    fReadConfigurationInfos = kTRUE;
  }
  else if (thresholdMethod == 3) { // Take values from file

    // choose default values to be used if block is absent from input file

    if (DataCard->CheckLAVThresholdNominalValuesProvided()) {
      NominalThresholdHigh = DataCard->GetLAVThresholdHighValue();
      NominalThresholdLow = DataCard->GetLAVThresholdLowValue();
      NominalHysteresis = DataCard->GetLAVHysteresisValue();
    }
    else {
      NominalThresholdHigh = fGeom->GetLeadingThresholdHigh();
      NominalThresholdLow = fGeom->GetLeadingThresholdLow();
      NominalHysteresis = fGeom->GetHysteresiLow();
    }
    for (Int_t i=0; i<MAXBLOCKMAP; i++) {
      fHighThreshold[i] = NominalThresholdHigh;
      fLowThreshold[i] = NominalThresholdLow;
      fHysteresis[i] = NominalHysteresis;
    }

    DoThresholdReadingFromFile(DataCard->GetLAVThresholdFileName());
  }
  else if (thresholdMethod == 4) { // Take values from DB
    DoThresholdReadingFromDB();
  }
  else if (thresholdMethod == 5) { // Will take values from a hardcoded table, here initialize to nominal values
    for (Int_t i=0; i<MAXBLOCKMAP; i++) {
      fHighThreshold[i] = NominalThresholdHigh;
      fLowThreshold[i] = NominalThresholdLow;
      fHysteresis[i] = NominalHysteresis;
    }
  }
  else {
    std::cerr << "LAVConfiguration Error: wrong thresholdMethod from DataCard " << thresholdMethod << std::endl;
    exit(kWrongConfiguration);
  }
}

LAVConfiguration * LAVConfiguration::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new LAVConfiguration(); }
  return fInstance;
}

void LAVConfiguration::DoThresholdReadingFromDB(){
  fReadConfigurationInfos = kFALSE; // DB still not implemented
}

void LAVConfiguration::DoThresholdReadingFromFile(TString LAVThresholdFileName){

  if(NA62ConditionsService::GetInstance()->Open(LAVThresholdFileName)!=kSuccess) return;

  Int_t blockInstances[MAXBLOCKMAP]={0};
  Int_t nBlockRead = 0;
  TString Line;
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(LAVThresholdFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray * l = Line.Tokenize(" ");
    Int_t numberOfStrings = l->GetEntriesFast();
    if (numberOfStrings != 4) {
      std::cerr << "LAVConfiguration: Wrong input string in LAV threshold file: " << numberOfStrings << std::endl;
      exit(kWrongConfiguration);
    }

    Int_t BlockID = ((TObjString*)(l->At(0)))->GetString().Atoi();
    if (BlockID < 0 || BlockID > MAXBLOCKMAP) {
      std::cerr << "Error in LAVConfiguration >> Threshold file corruption: " << BlockID;
      exit(kWrongConfiguration);
    }
    if (blockInstances[BlockID]) {
      std::cerr << "LAVConfiguration -- Double-Block: " << BlockID << std::endl;
      exit(kWrongConfiguration);
    }

    fHighThreshold[BlockID] = ((TObjString*)(l->At(1)))->GetString().Atof();
    fLowThreshold[BlockID]  = ((TObjString*)(l->At(2)))->GetString().Atof();
    fHysteresis[BlockID]    = ((TObjString*)(l->At(3)))->GetString().Atof();
    
    // will be re-enabled when a coherent verbosity control will be implemented
    // std::cout << "LAVConfiguration >> Read from input File " << BlockID << " " << fLowThreshold[BlockID] << " " << fHighThreshold[BlockID] << " " << fHysteresis[BlockID] << std::endl;
    
    blockInstances[BlockID]++;
    nBlockRead++;
    l->Delete();
  }
  NA62ConditionsService::GetInstance()->Close(LAVThresholdFileName);
  
  Int_t nSt = fGeom->GetNumberOfStations();
  Int_t nTotalBlocks = 0;
  for (Int_t iSt=0; iSt<nSt; iSt++) {
    Int_t nLay = fGeom->GetNumberOfLayers(iSt);
    Int_t nBanPerLay = fGeom->GetNumberOfBananas(iSt);

    nTotalBlocks += nLay*nBanPerLay*4;
  }

  if (nBlockRead != nTotalBlocks) {
    std::cout << "LAVConfiguration -- Warning Missing Blocks: " << nBlockRead << " " << nTotalBlocks << " " << LAVThresholdFileName.Data() << "using default values for those " << std::endl;
  }

  fReadConfigurationInfos = kTRUE;

}
