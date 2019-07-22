// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "SpectrometerCoarseT0.hh"

SpectrometerCoarseT0::SpectrometerCoarseT0(Core::BaseAnalysis *ba) : CoarseT0Evaluation(ba, "Spectrometer") {
  fNROMezzaninesPerFullBoard = 16; // SRB-readout
}
