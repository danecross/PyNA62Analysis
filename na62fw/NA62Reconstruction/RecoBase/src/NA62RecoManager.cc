// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2019-04-25
//
// ---------------------------------------------------------------

#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"
#include <iostream>
#include "TPRegexp.h"
#include "TObjArray.h"
#include "TObjString.h"
#include "TCedarSpecialTriggerEvent.hh"

NA62RecoManager* NA62RecoManager::fInstance = nullptr;

NA62RecoManager* NA62RecoManager::GetInstance(){
  if(!fInstance) fInstance = new NA62RecoManager();
  return fInstance;
}

NA62RecoManager::NA62RecoManager(){
  fEventHeader = new EventHeader();
} 

NA62RecoManager::~NA62RecoManager(){
  if(fEventHeader){
    delete fEventHeader;
    fEventHeader=nullptr;
  }
}

void NA62RecoManager::InitNA62ConditionsService(TString InputFileName){

  // Initialise NA62ConditionsService variables
  Int_t ConditionsServiceRunID   = 6610; //default
  Int_t ConditionsServiceBurstID =    0; //default

  TObjArray *subStrL;
  Bool_t IsRawData = ! InputFileName.EndsWith(".root");
  if(IsRawData) subStrL = TPRegexp("^.*-(\\d{6})-(\\d{4})(.calib)?.dat$").MatchS(InputFileName);
  else           subStrL = TPRegexp("^.*_dr(\\d{4,6})_r(\\d{6,8}).root$").MatchS(InputFileName);
  if(subStrL->GetLast()>=2){
    if(static_cast<TObjString *>(subStrL->At(1))->GetString().Atoi()>0) ConditionsServiceRunID = static_cast<TObjString *>(subStrL->At(1))->GetString().Atoi();
    if(IsRawData) ConditionsServiceBurstID = static_cast<TObjString *>(subStrL->At(2))->GetString().Atoi(); //ignore it for MC
  }
  else { //error: unknown format of the input file
    std::cerr << "[NA62RecoManager] WARNING: file name '" << InputFileName << "' has an unknown format.. Failed to extract current Run and Burst IDs!" << std::endl;
    std::cerr << "[NA62RecoManager]          --> ConditionsService will use the default run-burst IDs: " << Form("%06d-%04d",ConditionsServiceRunID,ConditionsServiceBurstID) << std::endl;
  }
  NA62ConditionsService::GetInstance()->SetCurrentRunID(ConditionsServiceRunID);
  NA62ConditionsService::GetInstance()->SetCurrentBurstID(ConditionsServiceBurstID);
}

void NA62RecoManager::StoreDIMBlock(const TString name, UInt_t * pDataBuffer, UInt_t NumberOfWords, TSpecialTriggerEvent *SpecTrigEvent){
  if(name=="DIM") {
    NA62RecoManager::GetInstance()->GetEventHeader()->SetBeamSpecialTrigger(pDataBuffer,NumberOfWords);

    std::cout << "EOB TimeStamp:         " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetTimeStamp()          << std::endl;
    std::cout << "T10 Intensity/E11:     " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetIntensityT10()       << std::endl;
    std::cout << "Counts [QX]:           " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetCountsQX()           << std::endl;
    std::cout << "Counts [Q1_OR]:        " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetCountsQ1_OR()        << std::endl;
    std::cout << "Counts [MUV1_OR_MUV2]: " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetCountsMUV1_OR_MUV2() << std::endl;
    std::cout << "Counts [MUV3]:         " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetCountsMUV3()         << std::endl;
    std::cout << "Counts [NHOD]:         " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetCountsNHOD()         << std::endl;
    std::cout << "Counts [IRC]:          " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetCountsIRC()          << std::endl;
    std::cout << "Counts [CHANTI]:       " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetCountsCHANTI()       << std::endl;
    std::cout << "Counts [ARGONION]:     " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetCountsARGONION()     << std::endl;
    std::cout << "BBQ (kHz)      [CHOD]: " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetBBQCHOD()            << std::endl;
    std::cout << "MeanRate (kHz) [CHOD]: " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetMeanRateCHOD()       << std::endl;
  }
  else if(name=="L1TP") {
    NA62RecoManager::GetInstance()->GetEventHeader()->SetL1TPSpecialTrigger(pDataBuffer);
  }
  else if(name=="L2EB") {
    NA62RecoManager::GetInstance()->GetEventHeader()->SetL2EBSpecialTrigger(pDataBuffer);
  }
  else if(name=="Cedar") {
    if(SpecTrigEvent) static_cast<TCedarSpecialTriggerEvent*>(SpecTrigEvent)->SetDIMInfo(pDataBuffer);
    else std::cerr << "[NA62Reconstruction] WARNING: Invalid SpecialTriggerEvent for '" << name << "': skipping DIM block" << std::endl;
  }
  else std::cerr << "[NA62Reconstruction] WARNING: no DIM decoding for detector '" << name << "': skipping DIM block" << std::endl;
}

Long_t NA62RecoManager::GetRunTimeFromDB(UInt_t RunNumber) {
  Long_t RunTime = -1;
  TString RunTimeStampInputFileName("RunTimes.dat");
  // exit if the file with run times is not found
  if (NA62ConditionsService::GetInstance()->Open(RunTimeStampInputFileName)!=kSuccess) {
    std::cout << "[NA62Reconstruction] Error: Run times file " << RunTimeStampInputFileName << " not found anywhere or empty" << std::endl;
    _exit(kConditionFileNotFound);
  }
  // file successfully opened
  Bool_t  RunFoundInRunTimeStampDB = false;
  UInt_t  RunNumberInFile;
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(RunTimeStampInputFileName)) &&	!RunFoundInRunTimeStampDB) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    // read the run number
    RunNumberInFile = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
    // extract the run time stamp for this run
    if (RunNumberInFile==RunNumber) {
      RunFoundInRunTimeStampDB = true;
      RunTime = static_cast<TObjString*>(l->At(1))->GetString().Atoll();
    }
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(RunTimeStampInputFileName);

  // exit from the reco if the run time is not found
  if (!RunFoundInRunTimeStampDB) {
    std::cout <<  "[NA62Reconstruction] Error: Run " << RunNumber << " not found in the file with run times "
      << RunTimeStampInputFileName << std::endl;
    _exit(kGenericError);
  }

  return RunTime;
}
