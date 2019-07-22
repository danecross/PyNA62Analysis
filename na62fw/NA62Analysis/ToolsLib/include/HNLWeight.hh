// ---------------------------------------------------------------
// History:                                                                                 
//                                                                                           
// Created by Lorenza Iacobuzio (lorenza.iacobuzio@cern.ch) February 2018            
//                       
// ---------------------------------------------------------------

#ifndef HNLWEIGHT_HH
#define HNLWEIGHT_HH

#include <stdlib.h>
#include <iostream>
#include <vector>
#include <TROOT.h>
#include <TChain.h>
#include "Rtypes.h"

extern std::vector<std::map<std::string, Double_t>> ComputeWeight(Event*, Double_t, Double_t, Double_t, Double_t, Double_t, Double_t, Int_t);

extern std::vector<std::map<std::string, Double_t>> fWeightContainer;

#endif
