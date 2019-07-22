// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "RICHCoarseT0.hh"

RICHCoarseT0::RICHCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "RICH") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}
