// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "NewCHODCoarseT0.hh"

NewCHODCoarseT0::NewCHODCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "NewCHOD") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}
