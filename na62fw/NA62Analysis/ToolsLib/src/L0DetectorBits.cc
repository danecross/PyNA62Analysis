// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-05-31
// ---------------------------------------------------------------

/// \class L0DetectorBits
/// \Brief
/// A helper class for L0PrimitiveHandler. It contains the run ranges and names for 
/// each primitive bit from a given detector. 
/// \EndBrief
/// A helper class for L0PrimitiveHandler. It decodes and stores the run ranges and names for 
/// each primitive bit of a given detector. The function GetBit(TString) is used 
/// by L0PrimitiveHandler to determine which bit of the primitive ID has been requested by the
/// user.
/// \Detailed

#include <iostream>

#include "L0DetectorBits.hh"
#include "L0TPData.hh"

L0DetectorBits::L0DetectorBits(TString inputline) : fL0Detector(-1), fFirstRun(-1), fLastRun(-1){

  Int_t WordCount=0;
  fDetectorBits.resize(15,TString(""));
  
  TString item;
  Int_t from=0;
  while(inputline.Tokenize(item, from, " ")){
    if(WordCount==0){
      if(item.Contains("RICH"))          fL0Detector=kL0RICH;
      else if(item.Contains("LAV"))      fL0Detector=kL0LAV;
      else if(item.Contains("MUV"))      fL0Detector=kL0MUV3;
      else if(item.Contains("NewCHOD"))  fL0Detector=kL0NewCHOD;
      else if(item.Contains("Calo"))     fL0Detector=kL0Calo;
      else if(item.Contains("CHOD"))     fL0Detector=kL0CHOD;
      else if(item.Contains("TALK"))     fL0Detector=kL0TALK;
      else if(item.Contains("L0TP"))     fL0Detector=7; // Special case!!
      else if(item.Contains("EMULATOR")) fL0Detector=8; // Special case!!
      else                               fL0Detector=-1;
    }
    else if(WordCount==1) fFirstRun = item.Atoi();
    else if(WordCount==2) fLastRun  = item.Atoi();
    else fDetectorBits[WordCount-3] = item;
    WordCount++;
  }
  return;
}

Int_t L0DetectorBits::GetL0Detector(){return fL0Detector;}
Int_t L0DetectorBits::GetFirstRun(){return fFirstRun;}
Int_t L0DetectorBits::GetLastRun(){return fLastRun;}

Int_t L0DetectorBits::GetBit(TString item){
  for(unsigned int i=0; i<fDetectorBits.size(); ++i){
    if(fDetectorBits[i].CompareTo(item)==0) return i;
  }
  return 9999;
}
TString L0DetectorBits::GetBit(Int_t bit){
  // bit is unsigned so can't be less than zero
  TString r = "";
  if(bit<0){
    std::cout << "[L0DetectorBits] Tried to get bit " << bit << " which is less than 0!" << std::endl;
  }
  else if(bit>int(fDetectorBits.size())){
    std::cout << "[L0DetectorBits] Tried to get bit " << bit;
    std::cout << " which is more than the number of detector bits, which is " << fDetectorBits.size() << std::endl;
  }
  else{
    r = fDetectorBits[bit];
  }
  return r;
}

void L0DetectorBits::Print(){
  std::cout << " detector "  << fL0Detector
	    << " first run " << fFirstRun
	    << " last run "  << fLastRun << std::endl;
  this->PrintBits();
  std::cout << std::endl;
}

void L0DetectorBits::PrintBits(){
  for(int i=0; i<15; ++i){
    std::cout << " " << fDetectorBits[i];
  }
  std::cout << std::endl;
}
