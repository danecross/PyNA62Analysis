// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-12-27
//
// ---------------------------------------------------------------

#include "NA62Utilities.hh"
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
} 

NA62Utilities::~NA62Utilities(){}

Double_t NA62Utilities::GetUnitFromString(TString String){
  if(fMapStringUnit.find(String)!=fMapStringUnit.end()){
    return fMapStringUnit.find(String)->second;
  }
  else std::cerr << "[NA62Utilities] WARNING: Unknown unit '" << String << "'!" << std::endl;

  return 0.;             
}
