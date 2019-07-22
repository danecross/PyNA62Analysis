// --------------------------------------------------------------
// History:
//
// Created by T. Spadaro (09/09/2014)
//
// --------------------------------------------------------------
/// \class LAVCalibration
/// \Brief
/// Class for the management of T0 and slewing corrections.
/// \EndBrief
/// \Detailed
/// The T0InputMode is retrieved by the DataCardMessenger. Values have the following meaning:
/// T0InputMode = 1--> use hard-coded values 
/// T0InputMode = 2--> use station-by-station values from user input 
/// T0InputMode = 3--> use block-by-block values in the input file provided 
/// T0InputMode = 4--> retrieve values from db (to be implemented)
/// \n
/// The T0 file include:
/// 1. Global T0 values per station
/// 2. Parameters of the quartic curves for residual slewing corrections: separate curves for high+low leading and for low leading+trailing 
/// 3. Fine T0 value per block
/// \EndDetailed

#include "Riostream.h"
#include "TString.h"
#include "TObjString.h"
#include "TRegexp.h"
#include "LAVCalibration.hh"
#include "LAVDataCardMessenger.hh"
#include "NA62ConditionsService.hh"
#include "NA62Global.hh"

LAVCalibration* LAVCalibration::fInstance = 0;

LAVCalibration::LAVCalibration() :
  fIsMC(0) // MC flag only used to apply the proper slewing corrections
{
  // Take defaults from LAVGeometry

  fGeom = LAVGeometry::GetInstance();

  fReadLAVT0s = kFALSE;
  for (Int_t i=0; i<MAXBLOCKMAP; i++) fT0Corr[i] = 0;
  fReadSlewPars = kFALSE;
  for (Int_t i=0; i<2; i++) 
    for (Int_t j=0; j<MAXPARSPERTHRESHOLD; j++) fSlewingPars[i][j] = 0;
  
  LAVDataCardMessenger* DataCard = LAVDataCardMessenger::GetInstance();
  Int_t T0InputMode = DataCard->GetLAVT0CorrectionInputMode();
  if (T0InputMode == 1) { // from default, nothing to do
    fReadLAVT0s = kTRUE;
  }
  else if (T0InputMode == 2) {

    if (!DataCard->CheckLAVT0NominalValueProvided()) { // from datacard values
      std::cout << "[LAVCalibration] Inconsistency from datacard" << std::endl;
      exit(kWrongConfiguration);
    }
    
    Int_t nSt = fGeom->GetNumberOfStations();
    for (Int_t iSt=0; iSt<nSt; iSt++) {
      Double_t stationT0 = DataCard->GetLAVT0CorrectionValue(iSt);
      
      Int_t nLay = fGeom->GetNumberOfLayers(iSt);
      Int_t nBanPerLay = fGeom->GetNumberOfBananas(iSt);
      for (Int_t iLay=0; iLay<nLay; iLay++){
	for (Int_t iBan=0; iBan<nBanPerLay; iBan++) {
	  for (Int_t iBlk=0; iBlk<4; iBlk++){
	    fT0Corr[(iSt+1)*10000+iLay*1000+iBan*10+iBlk] = stationT0;
	  }
	}
      }
    }
    fReadLAVT0s = kTRUE;
  }
  else if (T0InputMode == 3) { // from input file
    DoT0ReadingFromFile(DataCard->GetLAVT0CorrectionFileName());
  }  
  else if (T0InputMode == 4) { // from db
    DoT0ReadingFromDB();  
  }
  else {
    std::cout << "[LAVCalibration] Wrong T0InputMode from datacard " << T0InputMode << std::endl;
    exit(kWrongConfiguration);
  }
}

LAVCalibration * LAVCalibration::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new LAVCalibration(); }
  return fInstance;
}

void LAVCalibration::DoT0ReadingFromDB(){
  fReadLAVT0s = kFALSE;
  return;
}

void LAVCalibration::DoT0ReadingFromFile(TString LAVT0FileName){
  if(NA62ConditionsService::GetInstance()->Open(LAVT0FileName)!=kSuccess) return;
  //Double_t stationT0Value[12] = {0};
  Int_t blockInstances[MAXBLOCKMAP]={0};
  Int_t nBlockRead = 0;
  Bool_t stationMode = kFALSE;
  Bool_t blockMode = kFALSE;
  Bool_t slewingMode = kFALSE;
  Int_t nSlewingPars[2] = {0};
  TString Line;
  //Double_t GlobalTimeOffset = 0;
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(LAVT0FileName))) {
    if (Line.BeginsWith("#")) continue;
    if (Line.BeginsWith("Station")) {
      stationMode = kTRUE;
      blockMode = kFALSE;
      slewingMode = kFALSE;
      continue;
    }
    if (Line.BeginsWith("Block")) {
      stationMode = kFALSE;
      blockMode = kTRUE;
      slewingMode = kFALSE;
      continue;
    }
    if (Line.BeginsWith("ResidualSlewing")) {
      stationMode = kFALSE;
      blockMode = kFALSE;
      slewingMode = kTRUE;
      continue;
    }
    if (Line.BeginsWith("GlobalTimeOffset")) {
      TObjArray * l = Line.Tokenize(" ");
      Int_t numberOfStrings = l->GetEntriesFast();
      if (numberOfStrings != 2) {
	std::cout << "[LAVCalibration] GlobalTimeOffset input: wrong string in LAV T0 file: " << numberOfStrings << "---" << Line.Data() << std::endl;
	exit(kWrongConfiguration);
      }
      //GlobalTimeOffset = ((TObjString*)(l->At(1)))->GetString().Atof();
      delete l;
      continue;
    }

    TObjArray * l = Line.Tokenize(" ");
    Int_t numberOfStrings = l->GetEntriesFast();
    if (numberOfStrings != 2) {
      std::cout << "[LAVCalibration] Wrong input string in LAV T0 file: " << numberOfStrings << "---" << Line.Data() << std::endl;
      exit(kGenericError);
    }

// station Mode

    if (stationMode) {
      Int_t station = ((TObjString*)(l->At(0)))->GetString().Atoi();
      if (station < 1 || station > 12) {
	std::cout << "[LAVCalibration] Error, T0 file corruption: " << Line.Data();
	exit(kGenericError);
      }
      //stationT0Value[station-1] = ((TObjString*)(l->At(1)))->GetString().Atof();
    }

// station Mode

    if (slewingMode) {
      Int_t iThreshold = ((TObjString*)(l->At(0)))->GetString().Atoi();
      if (iThreshold < 0 || iThreshold > 1) {
	std::cout << "[LAVCalibration] Error, T0 file corruption in the slewing sector: " << Line.Data();
	exit(kGenericError);
      }
      if (nSlewingPars[iThreshold] == MAXPARSPERTHRESHOLD) {
	std::cout << "[LAVCalibration] Error, T0 file corruption in the slewing sector: too pars provided " << Line.Data();
	exit(kGenericError);
      }
      fSlewingPars[iThreshold][nSlewingPars[iThreshold]] = ((TObjString*)(l->At(1)))->GetString().Atof();
      nSlewingPars[iThreshold]++;
    }
    
// block Mode

    if (blockMode) {
      Int_t BlockID = ((TObjString*)(l->At(0)))->GetString().Atoi();
      if (BlockID < 0 || BlockID > MAXBLOCKMAP) {
	std::cout << "[LAVCalibration] Error, T0 file corruption: " << Line.Data();
	exit(kGenericError);
      }

      if (blockInstances[BlockID]) {
	std::cout << "[LAVCalibration] Error, Double-Block: " << BlockID << std::endl;
	exit(kGenericError);
      }
//      Int_t station = BlockID/10000;
      fT0Corr[BlockID] = ((TObjString*)(l->At(1)))->GetString().Atof();// + stationT0Value[station-1] - GlobalTimeOffset;
      //      fT0Corr[BlockID] = ((TObjString*)(l->At(1)))->GetString().Atof() + stationT0Value[station-1] - GlobalTimeOffset;
    
      // will be re-enabled when a coherent verbosity control will be implemented
      // std::cout << "[LAVCalibration] Read from input File " << BlockID << " " << fT0Corr[BlockID] << std::endl;

      blockInstances[BlockID]++;
      nBlockRead++;
    }
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(LAVT0FileName);
  
  Int_t nSt = fGeom->GetNumberOfStations();
  Int_t nTotalBlocks = 0;
  for (Int_t iSt=0; iSt<nSt; iSt++) {
    Int_t nLay = fGeom->GetNumberOfLayers(iSt);
    Int_t nBanPerLay = fGeom->GetNumberOfBananas(iSt);

    nTotalBlocks += nLay*nBanPerLay*4;
  }

  if (nBlockRead != nTotalBlocks) {
    std::cout << "[LAVCalibration] Warning Missing Blocks: " << nBlockRead << " " << nTotalBlocks << " " << LAVT0FileName.Data() << std::endl; // once per execution
  }
  fReadLAVT0s = kTRUE;
}

void LAVCalibration::AssignSlewingCorrections(Int_t IsMC){

  Int_t nSlewingPars[2] = {0};
  fReadSlewPars = kFALSE;
// If MC, only the slewing correction parameters in input in the datacard are used. If others are present in the T0 file, these are not used.

  if(IsMC==1){
    if (LAVDataCardMessenger::GetInstance()->CheckLAVMCSlewingCorrectionParametersProvided()) { // override input for slewing corrections from T0 file
      nSlewingPars[0] = 4; 
      nSlewingPars[1] = 5;
      for (Int_t iThreshold=0; iThreshold<2; iThreshold++) 
	for (Int_t j = 0; j < nSlewingPars[iThreshold]; j++)
	  fSlewingPars[iThreshold][j] = LAVDataCardMessenger::GetInstance()->GetLAVMCSlewingCorrectionParameter(iThreshold,j);
    }
    else {  // here, no slewing correction is applied for the MC, regardless any input from T0 file
      nSlewingPars[0] = 0;
      nSlewingPars[1] = 0;
    }
  }
  else {
    if (LAVDataCardMessenger::GetInstance()->CheckLAVDataSlewingCorrectionParametersProvided()) { // override input for slewing corrections from T0 file
      nSlewingPars[0] = 4; 
      nSlewingPars[1] = 5;
      for (Int_t iThreshold=0; iThreshold<2; iThreshold++) 
	for (Int_t j = 0; j < nSlewingPars[iThreshold]; j++)
	  fSlewingPars[iThreshold][j] = LAVDataCardMessenger::GetInstance()->GetLAVDataSlewingCorrectionParameter(iThreshold,j);
    }
    else {  // here, no slewing correction is applied for the Data, regardless any input from T0 file
      nSlewingPars[0] = 0;
      nSlewingPars[1] = 0;
    }
  }

  if (nSlewingPars[0] > 0 && nSlewingPars[1] > 0) fReadSlewPars = kTRUE;

  if (fReadSlewPars) {
    for (Int_t i=0; i<2; i++) {
      std::cout << "[LAVCalibration] " << nSlewingPars[i] << " slewing parameters available for threshold " << i;
      for (Int_t j=0; j<nSlewingPars[i]; j++) std::cout << " " << fSlewingPars[i][j];
      std::cout << " MC Flag = " << IsMC << std::endl;
    }
  }    
}
