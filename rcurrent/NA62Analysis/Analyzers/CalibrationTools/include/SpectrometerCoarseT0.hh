// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef SPECTROMETERCOARSET0_H
#define SPECTROMETERCOARSET0_H 1

class SpectrometerCoarseT0 : public CoarseT0Evaluation {

public:

  explicit SpectrometerCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerCoarseT0() {};
};

#endif
