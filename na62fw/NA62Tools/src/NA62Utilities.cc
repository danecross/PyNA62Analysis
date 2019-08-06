// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-12-27
//
// ---------------------------------------------------------------

#include "NA62Utilities.hh"
#include "NA62Global.hh"
#include "NA62ConditionsService.hh"
#include "TObjArray.h"
#include "TObjString.h"
#include <iostream>

NA62Utilities* NA62Utilities::fInstance = nullptr;

NA62Utilities* NA62Utilities::GetInstance(){
  if(!fInstance) fInstance = new NA62Utilities();
  return fInstance;
}

NA62Utilities::NA62Utilities() {
  // Fill the map
  TString Length[10] = {"nm","","","um", "","","mm", "cm","","m"  };
  TString Time[10]   = {"ns","","","us", "","","ms", "",  "","s"  };
  TString Energy[10] = {"eV","","","keV","","","MeV","",  "","GeV"};
  TString LengthExtended[10] = {"nanometer","","","micrometer","","","millimeter","centimeter","","meter"};
  TString TimeExtended[10]   = {"nanosecond","","","microsecond","","","millisecond","","","second"};
  TString EnergyExtended[10] = {"electronvolt","","","kiloelectronvolt","","","megaelectronvolt","","","gigaelectronvolt"};
  for(Int_t iPower=0; iPower<10; iPower++){
    fMapStringUnit.emplace(Length[iPower],pow(10.,iPower-6));         // wrt mm
    fMapStringUnit.emplace(LengthExtended[iPower],pow(10.,iPower-6)); // wrt mm
    fMapStringUnit.emplace(Time[iPower],  pow(10.,iPower));           // wrt ns 
    fMapStringUnit.emplace(TimeExtended[iPower],  pow(10.,iPower));   // wrt ns 
    fMapStringUnit.emplace(Energy[iPower],pow(10.,iPower-6));         // wrt MeV
    fMapStringUnit.emplace(EnergyExtended[iPower],pow(10.,iPower-6)); // wrt MeV
  }
  fRunTimesRead = false;
  for (Int_t i=0; i<20000; i++) fRunTime[i] = -1;
}

Double_t NA62Utilities::GetUnitFromString(TString String){
  if (fMapStringUnit.find(String)!=fMapStringUnit.end()) {
    return fMapStringUnit.find(String)->second;
  }
  std::cout << "[NA62Utilities] WARNING: Unknown unit '" << String << "'!" << std::endl;
  return 0.0;
}

Long_t NA62Utilities::GetRunTime(Int_t RunNumber) {

  if (!fRunTimesRead) {
    fRunTimesRead = true;
    TString FileName("RunTimes.dat");
    if (NA62ConditionsService::GetInstance()->Open(FileName)!=kSuccess) {
      std::cout << "[NA62Utilities] Error: Run times DB not found or empty" << std::endl;
      _exit(kConditionFileNotFound);
    }
    TString Line;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(FileName))) {
      if (Line.BeginsWith("#")) continue;
      TObjArray *l   = Line.Tokenize(" ");
      Int_t  RNumber = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
      Long_t RTime   = static_cast<TObjString*>(l->At(1))->GetString().Atoll();
      if (RNumber>=0 && RNumber<20000) fRunTime[RNumber] = RTime;
      delete l;
    }
    NA62ConditionsService::GetInstance()->Close(FileName);
  }

  if (RunNumber<0 || RunNumber>=20000) {
    std::cout << "[NA62Utilities] Invalid run number " << RunNumber << std::endl;
    _exit(kGenericError);
  }
  if (fRunTime[RunNumber]<0) {
    std::cout << "[NA62Utilities] Error: Run " << RunNumber << " not found in run times DB" << std::endl;
    _exit(kGenericError);
  }

  return fRunTime[RunNumber];
}
