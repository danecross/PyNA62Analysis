// ---------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-10-10
//
// ---------------------------------------------------------

#ifndef L0CHODEMULATOR_H
#define L0CHODEMULATOR_H

#include "L0RICHEmulator.hh"

class L0CHODEmulator : public L0RICHEmulator {

public:

  explicit L0CHODEmulator(NA62Analysis::Core::BaseAnalysis *ba);
  ~L0CHODEmulator() {};

  void FillTimes();
};

#endif
