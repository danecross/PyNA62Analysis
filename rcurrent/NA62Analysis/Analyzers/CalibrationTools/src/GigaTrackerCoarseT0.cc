// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "GigaTrackerCoarseT0.hh"

GigaTrackerCoarseT0::GigaTrackerCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "GigaTracker") {
  fNROMezzaninesPerFullBoard = 1; // GTK-readout
}
