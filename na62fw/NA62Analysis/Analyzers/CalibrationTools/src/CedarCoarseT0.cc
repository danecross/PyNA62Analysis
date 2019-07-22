// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CedarCoarseT0.hh"

CedarCoarseT0::CedarCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "Cedar") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}
