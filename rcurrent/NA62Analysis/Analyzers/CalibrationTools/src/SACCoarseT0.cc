// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "SACCoarseT0.hh"

SACCoarseT0::SACCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "SAC") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}

