// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-05-10
//
// ---------------------------------------------------------------

#ifndef LKRCALIBRATION_HH
#define LKRCALIBRATION_HH

#include <fstream>
#include <iostream>
#include "TObjString.h"
#include "TObjArray.h"

#define NBURMAX_LKRFINECALIB 5000

class LKrFineCalibration {

public:

  static LKrFineCalibration* GetInstance();
  Double_t GetEnergyScaling(Int_t, Int_t);
  Double_t GetEnergyOffset(Int_t, Int_t);
  void Print();

private:

  LKrFineCalibration();
  ~LKrFineCalibration() {}
  void ParseInputFile();

  TString fFileName; ///< Name of text file with burst-by-burst calibrations
  Int_t fRunID; ///< Run number: constants for one run only are stored in memory

  // Parameters of the function (E/p)(E) = p0 + p1/E for each burst
  Double_t fEnergyScaling[NBURMAX_LKRFINECALIB], fEnergyOffset[NBURMAX_LKRFINECALIB];
};

#endif
