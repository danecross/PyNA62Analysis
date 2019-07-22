// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CHODCoarseT0.hh"

CHODCoarseT0::CHODCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "CHOD") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}
