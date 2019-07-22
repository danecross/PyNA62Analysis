// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "MUV3CoarseT0.hh"

MUV3CoarseT0::MUV3CoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "MUV3") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}

