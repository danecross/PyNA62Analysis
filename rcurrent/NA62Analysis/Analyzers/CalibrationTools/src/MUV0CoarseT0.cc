// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "MUV0CoarseT0.hh"

MUV0CoarseT0::MUV0CoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "MUV0") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}
