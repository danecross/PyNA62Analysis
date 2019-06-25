// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "LAVCoarseT0.hh"

LAVCoarseT0::LAVCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "LAV") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}
