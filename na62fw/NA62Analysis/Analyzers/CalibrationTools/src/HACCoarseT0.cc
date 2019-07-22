// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "HACCoarseT0.hh"

HACCoarseT0::HACCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "HAC") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}
