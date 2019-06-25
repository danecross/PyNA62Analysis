// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson) 2017-05-31
// ---------------------------------------------------------------

#ifndef L0DETECTORBITS_HH
#define L0DETECTORBITS_HH

#include "TString.h"

class L0DetectorBits{

 public:

  explicit L0DetectorBits(TString);

  Int_t GetL0Detector(); 
  Int_t GetFirstRun();
  Int_t GetLastRun(); 
  Int_t GetBit(TString);
  TString GetBit(Int_t);
  void Print();
  void PrintBits();

 private:
  Int_t fL0Detector;
  Int_t fFirstRun; 
  Int_t fLastRun;
  std::vector<TString> fDetectorBits;
};

#endif
