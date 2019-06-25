// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CHANTICoarseT0.hh"

CHANTICoarseT0::CHANTICoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "CHANTI") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}
