// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "IRCCoarseT0.hh"

IRCCoarseT0::IRCCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "IRC") {
  fNROMezzaninesPerFullBoard = 4; // TEL62-readout
}
